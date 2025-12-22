# =========================================================
# 1. Helper functions
# =========================================================
download_laz_parallel <- function(urls,
                                  output_dir,
                                  cores = NULL,
                                  reserve_cores = 6L,
                                  timeout = 600) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  laz_dir <- file.path(output_dir, "LAZ")
  if (!dir.exists(laz_dir)) dir.create(laz_dir, recursive = TRUE, showWarnings = FALSE)
  
  urls <- trimws(gsub(",+$", "", urls))
  urls <- unique(urls[nzchar(urls)])
  if (length(urls) == 0) stop("No valid URLs provided.")
  
  total_cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) 1L)
  if (is.null(cores)) {
    workers <- max(1L, total_cores - reserve_cores)
  } else {
    workers <- min(as.integer(cores), total_cores)
  }
  if (workers < 1L) workers <- 1L
  
  message("🔧 Total detected cores: ", total_cores)
  message("🔧 Workers used: ", workers)
  
  old_timeout <- getOption("timeout")
  on.exit({ options(timeout = old_timeout) }, add = TRUE)
  options(timeout = timeout)
  
  old_plan <- future::plan()
  on.exit({ future::plan(old_plan) }, add = TRUE)
  future::plan(future::multisession, workers = workers)
  
  existing_files <- list.files(laz_dir, pattern = "\\.laz$", full.names = FALSE, ignore.case = TRUE)
  urls_to_download <- urls[!(basename(urls) %in% existing_files)]
  
  n_existing <- sum(basename(urls) %in% existing_files)
  n_new <- length(urls_to_download)
  
  message("✅ LAZ files already present in 'LAZ': ", n_existing)
  message("⬇ LAZ files to download now: ", n_new)
  
  download_tile <- function(url, laz_dir) {
    file_name <- basename(url)
    destfile  <- file.path(laz_dir, file_name)
    
    out <- tryCatch({
      resp_check <- httr::HEAD(url)
      code <- httr::status_code(resp_check)
      
      if (code == 200) {
        httr::GET(url, httr::write_disk(destfile, overwrite = TRUE), httr::progress())
        c(file_name, "Downloaded", url)
      } else {
        c(file_name, paste0("Failed (", code, ")"), url)
      }
    }, error = function(e) {
      c(file_name, paste0("Error: ", e$message), url)
    })
    out
  }
  
  results <- list()
  if (n_new > 0) {
    results <- future.apply::future_lapply(urls_to_download, download_tile, laz_dir = laz_dir)
  }
  
  log_status <- data.frame(
    filename = c(
      basename(urls[basename(urls) %in% existing_files]),
      if (length(results) > 0) vapply(results, `[[`, character(1), 1) else character(0)
    ),
    status = c(
      rep("Already exists", n_existing),
      if (length(results) > 0) vapply(results, `[[`, character(1), 2) else character(0)
    ),
    source = c(
      rep(NA_character_, n_existing),
      if (length(results) > 0) vapply(results, `[[`, character(1), 3) else character(0)
    ),
    stringsAsFactors = FALSE
  )
  
  log_path <- file.path(laz_dir, "FILE_PROCESSING.txt")
  write.table(log_status, file = log_path, row.names = FALSE, sep = "\t", quote = FALSE)
  message("✅ Log written to: ", log_path)
  
  invisible(log_status)
}

# Extract project/source name from 'titles' column (USGS lidar)
extract_source_from_titles <- function(titles) {
  if (length(titles) == 0) return(character(0))
  s <- sub("^USGS Lidar Point Cloud\\s+", "", titles)
  vapply(strsplit(s, "\\s+"), `[`, character(1), 1)
}

# =========================
# LiDAR tiling + tiled search
# =========================

split_aoi_5km <- function(aoi_ll, tile_size_m = 5000, metric_crs = 5070) {
  stopifnot(sf::st_is_longlat(aoi_ll))
  
  aoi_m <- sf::st_transform(aoi_ll, metric_crs)
  
  grid <- sf::st_make_grid(
    aoi_m,
    cellsize = c(tile_size_m, tile_size_m),
    square   = TRUE
  ) |>
    sf::st_as_sf() |>
    dplyr::mutate(tile_id = dplyr::row_number())
  
  # keep only tiles that intersect AOI (and clip to AOI)
  grid <- grid[sf::st_intersects(grid, aoi_m, sparse = FALSE), ]
  grid <- sf::st_intersection(grid, sf::st_geometry(aoi_m))
  
  sf::st_transform(grid, 4326)  # back to lon/lat for bbox API
}

filter_unique_tiles_latest <- function(df, key_col = "titles") {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (!key_col %in% names(df)) stop("filter_unique_tiles_latest(): key_col not found: ", key_col)
  
  df |>
    dplyr::group_by(.data[[key_col]]) |>
    dplyr::arrange(dplyr::desc(startYear), dplyr::desc(sizeInBytes)) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
}

# main function to replace single lidar_search()
run_lidar_search_tiled <- function(aoi_ll,
                                   tile_size_m = 5000,
                                   max_return  = 10000,
                                   metric_crs  = 5070,
                                   grid_id     = NA_integer_,
                                   preview     = FALSE,
                                   show_map    = TRUE,
                                   return_unique = TRUE,
                                   unique_key_col = "titles") {
  
  tiles_ll <- split_aoi_5km(aoi_ll, tile_size_m = tile_size_m, metric_crs = metric_crs)
  n_tiles  <- nrow(tiles_ll)
  
  message("Number of tiles: ", n_tiles)
  
  # optional preview map (only if mapview installed)
  if (isTRUE(show_map) && requireNamespace("mapview", quietly = TRUE)) {
    m <- mapview::mapview(aoi_ll, col.regions = NA, color = "black", lwd = 2) +
      mapview::mapview(tiles_ll, alpha.regions = 0.25, color = "blue", lwd = 1,
                       label = tiles_ll$tile_id)
    print(m)
  }
  
  out <- vector("list", n_tiles)
  
  for (j in seq_len(n_tiles)) {
    message("\n=============================")
    message("Processing tile ", sprintf("%04d", j), " of ", n_tiles,
            " | mother GRID: ", grid_id)
    
    bb <- sf::st_bbox(tiles_ll[j, ])
    bbox_vec <- c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    print(bbox_vec)
    
    res <- tryCatch(
      dsmSearch::lidar_search(
        bbox       = bbox_vec,
        max_return = max_return,
        preview    = preview
      ),
      error = function(e) NULL
    )
    
    if (is.null(res) || (is.data.frame(res) && nrow(res) == 0)) {
      out[[j]] <- NULL
      next
    }
    
    out[[j]] <- res |>
      dplyr::mutate(
        mother_grid = grid_id,
        tile_id     = tiles_ll$tile_id[j],
        xmin = bbox_vec[1], ymin = bbox_vec[2], xmax = bbox_vec[3], ymax = bbox_vec[4]
      )
  }
  
  res_all <- dplyr::bind_rows(out)
  
  if (isTRUE(return_unique)) {
    res_unique <- filter_unique_tiles_latest(res_all, key_col = unique_key_col)
    attr(res_unique, "res_all")  <- res_all
    attr(res_unique, "n_tiles")  <- n_tiles
    return(res_unique)
  } else {
    attr(res_all, "n_tiles") <- n_tiles
    return(res_all)
  }
}

