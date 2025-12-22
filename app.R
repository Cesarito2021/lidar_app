# app.R ---------------------------------------------------------------
suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(sf))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(DT))
suppressMessages(library(dsmSearch))
suppressMessages(library(future))
suppressMessages(library(future.apply))
suppressMessages(library(httr))
suppressMessages(library(parallel))

options(shiny.maxRequestSize = 200 * 1024^2)  # 200 MB upload

# =========================================================
# 1. Helper functions (from base.R)
# =========================================================
app_dir <- getwd()
source(file.path(app_dir, "base.R"))

# =========================================================
# 2. Local helpers
# =========================================================
read_aoi_any <- function(fileinfo) {
  ext <- tolower(tools::file_ext(fileinfo$name[1]))
  tmpdir <- tempdir()
  
  if (ext == "zip") {
    unzip(fileinfo$datapath[1], exdir = tmpdir)
    shp_files <- list.files(tmpdir, pattern = "\\.shp$", full.names = TRUE)
    validate(need(length(shp_files) > 0, "No .shp file found inside the zip."))
    sf::st_read(shp_files[1], quiet = TRUE)
  } else {
    sf::st_read(fileinfo$datapath[1], quiet = TRUE)
  }
}

get_downloads_dir <- function() {
  # Windows
  up <- Sys.getenv("USERPROFILE")
  if (nzchar(up)) {
    d <- file.path(up, "Downloads")
    if (dir.exists(d)) return(normalizePath(d, winslash = "/", mustWork = FALSE))
  }
  # macOS/Linux
  h <- normalizePath("~", winslash = "/", mustWork = FALSE)
  d2 <- file.path(h, "Downloads")
  if (dir.exists(d2)) return(normalizePath(d2, winslash = "/", mustWork = FALSE))
  # fallback
  h
}

sanitize_folder_name <- function(x) {
  x <- trimws(x)
  if (!nzchar(x)) return("AOI")
  gsub("[^A-Za-z0-9_-]", "_", x)
}

# =========================================================
# 3. UI (LOCAL-ONLY)
# =========================================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "ALS (LiDAR)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("ALS download (LAZ)", tabName = "als_dl", icon = icon("cloud-download"))
    )
  ),
  dashboardBody(
    tags$head(
      if (file.exists("www/css/main.css")) includeCSS("www/css/main.css")
    ),
    tabItems(
      tabItem(
        tabName = "als_dl",
        fluidPage(
          titlePanel("ALS (LiDAR) search & download"),
          sidebarLayout(
            sidebarPanel(
              width = 4,
              
              fileInput(
                "aoi_file",
                "1) Upload AOI (.zip shapefile or .gpkg/.geojson):",
                multiple = FALSE,
                accept = c(".zip", ".gpkg", ".geojson")
              ),
              uiOutput("aoi_field_ui"),
              uiOutput("aoi_values_ui"),
              
              hr(),
              
              helpText("Local-only mode: multicore download. Files are saved to your PC."),
              
              numericInput(
                "n_cores",
                "Download cores:",
                value = max(1, parallel::detectCores() - 1),
                min = 1,
                max = max(1, parallel::detectCores()),
                step = 1
              ),
              
              textInput(
                "base_dir",
                "Output folder directory:",
                value = get_downloads_dir()
              ),
              
              textInput("aoi_label", "Output folder name:", value = "Output"),
              
              actionButton(
                "run_search",
                "2) Search LiDAR tiles for this AOI",
                icon = icon("search"),
                style = "color:#0b1120; background-color:#38bdf8; border-color:#38bdf8; width:100%;"
              ),
              hr(),
              
              uiOutput("year_selector_ui"),
              uiOutput("source_selector_ui"),
              
              actionButton(
                "run_download",
                "4) Download selected tiles to LAZ folder",
                icon = icon("download"),
                style = "color: #0b1120; background-color: #22c55e; border-color: #22c55e; width:100%;"
              ),
              hr(),
              verbatimTextOutput("status_text")
            ),
            mainPanel(
              width = 8,
              
              h4("LiDAR Coverage Overview"),
              div(
                style = "background-color:#020617; padding:6px; border-radius:4px;
                         box-shadow:0 2px 4px rgba(0,0,0,0.6);",
                tags$img(
                  src = "logo/logo_300dpi.png",
                  style = "width:100%; height:auto; border-radius:3px;"
                )
              ),
              br(),
              h4("Tile availability by year"),
              plotOutput("year_barplot", height = 260),
              br(),
              h4("Summary (Year x Source)"),
              DTOutput("tiles_summary_table"),
              br(),
              h4("Full tile table"),
              DTOutput("tiles_full_table")
            )
          )
        )
      )
    )
  )
)

# =========================================================
# 4. SERVER
# =========================================================
server <- function(input, output, session) {
  
  res_lidar <- reactiveVal(NULL)
  
  aoi_sf <- reactive({
    req(input$aoi_file)
    read_aoi_any(input$aoi_file)
  })
  
  aoi_ll <- reactive({
    sf::st_transform(aoi_sf(), 4326)
  })
  
  output$aoi_field_ui <- renderUI({
    sf_obj <- aoi_ll()
    req(sf_obj)
    
    if (nrow(sf_obj) <= 1) {
      return(helpText("AOI has 1 feature. Entire geometry will be used."))
    }
    
    non_geom_cols <- names(sf_obj)[!sapply(sf_obj, inherits, what = "sfc")]
    if (!length(non_geom_cols)) {
      return(helpText("AOI has multiple polygons but no attribute columns (besides geometry)."))
    }
    
    selectInput(
      "aoi_field",
      "Polygon ID / name column:",
      choices = non_geom_cols,
      selected = non_geom_cols[1]
    )
  })
  
  output$aoi_values_ui <- renderUI({
    sf_obj <- aoi_ll()
    req(sf_obj)
    
    if (nrow(sf_obj) <= 1) return(NULL)
    
    req(input$aoi_field)
    vals <- sort(unique(sf_obj[[input$aoi_field]]))
    
    selectInput(
      "aoi_values",
      "Select polygon(s):",
      choices = vals,
      selected = vals[1],
      multiple = TRUE
    )
  })
  
  observeEvent(input$run_search, {
    req(aoi_ll())
    sf_obj <- aoi_ll()
    
    aoi_i <- sf_obj
    if (nrow(sf_obj) > 1 && !is.null(input$aoi_field) && !is.null(input$aoi_values)) {
      aoi_i <- sf_obj[sf_obj[[input$aoi_field]] %in% input$aoi_values, ]
    }
    
    if (nrow(aoi_i) == 0) {
      res_lidar(NULL)
      output$status_text <- renderText("❌ No AOI polygon found.")
      return()
    }
    
    withProgress(message = "Searching LiDAR tiles…", value = 0, {
      res <- suppressWarnings(
        try(
          run_lidar_search_tiled(
            aoi_ll         = aoi_i,
            tile_size_m    = 5000,
            max_return     = 10000,
            metric_crs     = 5070,
            grid_id        = 1,
            preview        = FALSE,
            show_map       = FALSE,
            return_unique  = TRUE,
            unique_key_col = "titles"
          ),
          silent = TRUE
        )
      )
    })
    
    if (inherits(res, "try-error") || is.null(res) || NROW(res) == 0) {
      res_lidar(NULL)
      output$status_text <- renderText("❌ No LiDAR tiles found (or error).")
      return()
    }
    
    res_lidar(res)
    
    n_tiles <- attr(res, "n_tiles"); if (is.null(n_tiles)) n_tiles <- NA
    yrs  <- sort(unique(res$startYear))
    srcs <- sort(unique(extract_source_from_titles(res$titles)))
    
    output$status_text <- renderText(
      paste0(
        "✅ Found ", nrow(res), " unique tiles\n",
        "Search tiles used: ", n_tiles, "\n",
        "Years: ", paste(yrs, collapse = ", "), "\n",
        "Sources: ", paste(srcs, collapse = ", "), "\n\n",
        "Output base dir: ", normalizePath(trimws(input$base_dir), winslash = "/", mustWork = FALSE)
      )
    )
  })
  
  output$year_selector_ui <- renderUI({
    # NOTE: this stays AFTER a search
    res <- res_lidar()
    if (is.null(res) || nrow(res) == 0) return(helpText("Run the LiDAR search first."))
    
    yrs <- sort(unique(res$startYear))
    if (exists("allowed_years_download") && !is.null(allowed_years_download)) {
      yrs <- yrs[yrs %in% allowed_years_download]
    }
    
    selectInput(
      "year_to_download",
      "Select year(s) to download (or ALL):",
      choices = c("ALL" = "ALL", setNames(yrs, yrs)),
      selected = "ALL",
      multiple = TRUE
    )
  })
  
  output$source_selector_ui <- renderUI({
    res <- res_lidar()
    if (is.null(res) || nrow(res) == 0) return(NULL)
    
    srcs <- sort(unique(extract_source_from_titles(res$titles)))
    
    selectInput(
      "source_to_download",
      "Select source/project(s) (or ALL):",
      choices = c("ALL" = "ALL", setNames(srcs, srcs)),
      selected = "ALL",
      multiple = TRUE
    )
  })
  
  output$year_barplot <- renderPlot({
    res <- res_lidar()
    if (is.null(res) || nrow(res) == 0) return(NULL)
    
    df_plot <- res %>%
      dplyr::group_by(startYear) %>%
      dplyr::summarise(n_tiles = dplyr::n(), .groups = "drop")
    
    ggplot(df_plot, aes(x = factor(startYear), y = n_tiles)) +
      geom_col(fill = "#fbbf24", color = "#fbbf24", width = 0.7) +
      labs(x = "Year", y = "Number of tiles") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background   = element_rect(fill = "#050816", colour = NA),
        panel.background  = element_rect(fill = "#020617", colour = "#020617"),
        panel.grid.major  = element_line(color = "#111827"),
        panel.grid.minor  = element_blank(),
        axis.text.x       = element_text(angle = 45, hjust = 1, colour = "#e5e7eb"),
        axis.text.y       = element_text(colour = "#e5e7eb"),
        axis.title        = element_text(colour = "#e5e7eb"),
        panel.border      = element_rect(colour = "#111827", fill = NA),
        legend.position   = "none"
      )
  })
  
  output$tiles_summary_table <- renderDT({
    res <- res_lidar()
    if (is.null(res) || nrow(res) == 0) return(NULL)
    
    df_sum <- res %>%
      mutate(
        basename = basename(downloadLazURL),
        source   = extract_source_from_titles(titles)
      ) %>%
      group_by(startYear, source) %>%
      summarise(
        n_tiles      = n(),
        example_tile = basename[1],
        .groups      = "drop"
      ) %>%
      arrange(startYear, source)
    
    datatable(
      df_sum,
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$tiles_full_table <- renderDT({
    res <- res_lidar()
    if (is.null(res) || nrow(res) == 0) return(NULL)
    
    df_full <- res %>%
      mutate(
        year     = startYear,
        basename = basename(downloadLazURL),
        source   = extract_source_from_titles(titles)
      ) %>%
      select(year, source, basename, downloadLazURL, titles)
    
    datatable(
      df_full,
      selection = "single",
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  observeEvent(input$run_download, {
    res <- res_lidar()
    req(res)
    
    # -------- 0. Validazione output dir e core --------
    base_dir_input <- trimws(input$base_dir)
    if (!nzchar(base_dir_input)) {
      showNotification("Please type an output folder path in 'Output folder directory'.", type = "error")
      output$status_text <- renderText("❌ No output folder directory specified.")
      return()
    }
    
    base_dir <- normalizePath(base_dir_input, winslash = "/", mustWork = FALSE)
    dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
    
    ncores <- suppressWarnings(as.integer(input$n_cores))
    if (is.na(ncores) || ncores < 1L) ncores <- 1L
    
    # -------- 1. Filtra anni e sorgenti --------
    yrs_all <- sort(unique(res$startYear))
    if (exists("allowed_years_download") && !is.null(allowed_years_download)) {
      yrs_all <- yrs_all[yrs_all %in% allowed_years_download]
    }
    src_all <- sort(unique(extract_source_from_titles(res$titles)))
    
    sel_year <- input$year_to_download
    if (is.null(sel_year) || length(sel_year) == 0) {
      showNotification("Please select at least one year or ALL.", type = "error")
      return()
    }
    years_to_download <- if ("ALL" %in% sel_year) yrs_all else as.integer(sel_year)
    
    sel_src <- input$source_to_download
    if (is.null(sel_src) || length(sel_src) == 0) {
      showNotification("Please select at least one source/project or ALL.", type = "error")
      return()
    }
    sources_to_download <- if ("ALL" %in% sel_src) src_all else sel_src
    
    res_sel <- res %>%
      dplyr::mutate(source = extract_source_from_titles(titles)) %>%
      dplyr::filter(
        startYear %in% years_to_download,
        source   %in% sources_to_download
      )
    
    if (nrow(res_sel) == 0) {
      showNotification("No tiles for the selected year(s)/source(s).", type = "error")
      return()
    }
    
    urls <- unique(res_sel$downloadLazURL)
    
    # -------- 2. Output folder per AOI --------
    aoi_label <- sanitize_folder_name(input$aoi_label)
    output_dir <- file.path(base_dir, aoi_label)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    # -------- 3. Multicore future plan (LOCAL) --------
    future::plan(future::multisession, workers = ncores)
    on.exit(future::plan(future::sequential), add = TRUE)
    
    # -------- 4. Download parallelo --------
    withProgress(message = "Downloading LAZ tiles...", value = 0, {
      log_status <- tryCatch(
        download_laz_parallel(
          urls       = urls,
          output_dir = output_dir,
          cores      = ncores
        ),
        error = function(e) {
          message("Download error: ", conditionMessage(e))
          NULL
        }
      )
      
      if (is.null(log_status)) {
        output$status_text <- renderText(
          paste0(
            "❌ Error during download. Check console/log.\n",
            "Tried to use folder: ", output_dir
          )
        )
      } else {
        output$status_text <- renderText(
          paste0(
            "✅ Download completed using ", ncores, " core(s).\n",
            "Tiles requested: ", length(urls), "\n",
            "LAZ saved under: ", file.path(output_dir, "LAZ")
          )
        )
      }
    })
  })
}

shinyApp(ui, server)
