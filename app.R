# app.R (Stage 4)
suppressPackageStartupMessages({
  library(shiny)
  library(mapgl)
  library(sf)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(rvest)
  library(jsonlite)
  library(stringr)
  library(glue)
  library(DT)
})

# == reuse the Stage 3 helpers verbatim ==
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
pick1 <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  x <- x[!is.na(x) & nzchar(as.character(x))]
  if (length(x)) x[1] else NA_character_
}

# (copy fetch_store_data and build_waffle_sf from Stage 3 here)
# --- BEGIN COPY ---
fetch_store_data <- function(url = "https://locations.wafflehouse.com") {
  page <- read_html(url)
  next_data <- page |>
    html_element("script#__NEXT_DATA__") |>
    html_text() |>
    jsonlite::fromJSON()
  stores_raw <- purrr::pluck(next_data, "props", "pageProps", "locations") %||%
    purrr::pluck(next_data, "props", "pageProps", "initialProps", "locations")
  if (is.null(stores_raw)) stop("No locations in __NEXT_DATA__")
  stores_raw |>
    tidyr::unnest(addressLines, keep_empty = TRUE) |>
    tidyr::unnest(custom, keep_empty = TRUE) |>
    mutate(
      longitude = suppressWarnings(as.numeric(longitude)),
      latitude = suppressWarnings(as.numeric(latitude)),
      formattedBusinessHours = purrr::map_chr(formattedBusinessHours, function(x) {
        if (is.null(x)) {
          return("")
        }
        if (is.character(x)) {
          return(paste(x, collapse = "; "))
        }
        if (is.list(x)) {
          return(paste(unlist(x, use.names = FALSE), collapse = "; "))
        }
        as.character(x)
      }),
      `_status` = coalesce(`_status`, NA_character_),
      status_simple = case_when(
        # First check for 24-hour operation
        str_detect(tolower(formattedBusinessHours), "24") ~ "Open 24h",
        # Interpret _status codes based on Waffle House Index
        `_status` == "A" ~ "Open (Green)", # Available/Active - full menu
        `_status` == "CT" ~ "Closed Temporarily (Red)", # Closed - disaster severe
        # If we have formattedBusinessHours but no status, assume open with limited menu
        !is.na(`_status`) & nzchar(`_status`) ~ paste0("Status: ", str_to_title(`_status`)),
        nzchar(formattedBusinessHours) ~ "Open (Yellow)", # Has hours = limited menu
        TRUE ~ "Status Unknown"
      ),
      phoneNumbers = map_chr(phoneNumbers, ~ {
        if (is.null(.x)) {
          return(NA_character_)
        }
        if (is.list(.x)) .x <- unlist(.x, use.names = FALSE)
        paste0(.x, collapse = ", ")
      })
    ) |>
    filter(!is.na(longitude), !is.na(latitude))
}
build_waffle_sf <- function() {
  df0 <- fetch_store_data()
  df <- df0 |>
    select(businessName, addressLines, city, state, postalCode,
      longitude, latitude, status_simple, phoneNumbers,
      online_order = online_order_link, url = websiteURL
    )
  key_cols <- c("businessName", "city", "state", "postalCode", "longitude", "latitude")
  df <- group_by(df, across(all_of(key_cols))) |>
    summarise(
      addressLines = paste(unique(na.omit(addressLines)), collapse = " "),
      status_simple = pick1(status_simple),
      phoneNumbers = pick1(phoneNumbers),
      online_order = pick1(online_order),
      url = pick1(url),
      .groups = "drop"
    ) |>
    mutate(tooltip_html = glue(
      "<div style='font-family:system-ui,Arial;min-width:220px'>
             <div style='font-weight:700'>{businessName}</div>
             <div>{addressLines %||% ''}{ifelse(nzchar(city %||% ''), paste0(', ', city), '')}{ifelse(nzchar(state %||% ''), paste0(', ', state), '')} {postalCode %||% ''}</div>
             <div><b>Status:</b> {status_simple %||% 'Unknown'}</div>
             <div><b>Phone:</b> {phoneNumbers %||% ''}</div>
             <div><a href='{online_order %||% url %||% '#'}' target='_blank'>Order / Website</a></div>
           </div>"
    ))

  df <- df |>
    mutate(fid = as.character(dplyr::row_number()))
  st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
}
# --- END COPY ---

ui <- fluidPage(
  theme = bslib::bs_theme(
    base_font = bslib::font_google("Quicksand"),
    primary = "#f8b500"
  ),
  tags$head(
    tags$style(HTML("
      /* Info panel in top-right, below clear button */
      #info_panel {
        position: absolute;
        top: 90px;
        right: 15px;
        z-index: 1000;
        max-width: 380px;
        background: white;
        border-radius: 12px;
        box-shadow: 0 8px 24px rgba(0,0,0,0.18);
        overflow: hidden;
        display: flex;
        flex-direction: column;
        max-height: calc(100vh - 200px);
        transition: max-height 0.3s ease;
      }
      #info_panel.collapsed {
        max-height: 52px;
      }
      #info_panel_header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 12px 16px;
        background: #f8b500;
        cursor: pointer;
        user-select: none;
      }
      #info_panel_header h4 {
        margin: 0;
        font-size: 16px;
        font-weight: 700;
        color: #333;
      }
      #info_panel_header .toggle-icon {
        font-size: 18px;
        color: #333;
        transition: transform 0.3s ease;
      }
      #info_panel_header .toggle-icon.collapsed {
        transform: rotate(-90deg);
      }
      #info_panel_content {
        padding: 16px;
        font-size: 14px;
        line-height: 1.6;
        overflow-y: auto;
        flex: 1;
        transition: flex 0.3s ease, padding 0.3s ease, opacity 0.3s ease;
      }
      #info_panel_content.collapsed {
        flex: 0;
        padding-top: 0;
        padding-bottom: 0;
        opacity: 0;
        overflow: hidden;
      }
      #info_panel_content p {
        margin: 0 0 10px 0;
        color: #444;
      }
      #info_panel_content p:last-child {
        margin-bottom: 0;
      }
      /* Clear button in top-right */
      #clear_btn {
        position: absolute;
        top: 15px;
        right: 15px;
        z-index: 1000;
      }
      /* COVID button below clear button */
      #covid_btn {
        position: absolute;
        top: 55px;
        right: 15px;
        z-index: 1000;
      }
      .mapboxgl-popup-content { padding: 10px 12px !important; }

      /* Expanded table panel (bottom-right) */
  #sel_panel {
    position: absolute;
    right: 15px;
    bottom: 15px;
    z-index: 1000;
    width: 500px;        /* was 420px — widened for link visibility */
    max-height: 42vh;
    overflow: hidden;
    background: white;
    border-radius: 12px;
    box-shadow: 0 8px 24px rgba(0,0,0,0.18);
    padding: 10px;
  }

  #sel_panel .dataTables_wrapper .dataTables_scrollBody {
    max-height: 34vh !important;
  }

  #sel_panel_collapsed {
    position: absolute;
    right: 15px;
    bottom: 50px;        /* lifted above map attribution */
    z-index: 1100;
    background: white;
    border-radius: 999px;
    box-shadow: 0 8px 24px rgba(0,0,0,0.18);
    padding: 8px 14px;
    font-weight: 600;
    color: #444;
    cursor: default;
  }
    "))
  ),

  # Full-screen map
  maplibreOutput("map", height = "100vh"),

  # Info panel (left side, collapseable)
  absolutePanel(
    id = "info_panel",
    div(
      id = "info_panel_header",
      onclick = "toggleInfoPanel()",
      h4("About the Waffle House Index"),
      tags$span(class = "toggle-icon", HTML("&#9660;")) # down arrow
    ),
    div(
      id = "info_panel_content",
      p(
        "The ", tags$strong("Waffle House Index"), " is an informal metric used by FEMA to assess the severity of a disaster:"
      ),
      tags$ul(
        style = "margin: 8px 0; padding-left: 20px;",
        tags$li(tags$strong(style = "color: #28a745;", "Green (Open):"), " Full menu available, minimal damage"),
        tags$li(tags$strong(style = "color: #ffc107;", "Yellow (Limited Menu):"), " Power outage or limited supplies"),
        tags$li(tags$strong(style = "color: #dc3545;", "Red (Closed):"), " Severe damage, unsafe conditions")
      ),
      p(
        "Waffle House restaurants are known for their disaster preparedness with backup generators, stocked supplies, and trained staff, ",
        "making closure a significant indicator of severity."
      ),
      p(
        tags$strong("This app"), " displays live Waffle House locations scraped from their official website. ",
        "Use the lasso or rectangle tool (top-left) to select stores and explore details. Export selections as CSV."
      ),
      p(
        tags$strong("Historical Note:"), " During the COVID-19 pandemic, unprecedented Waffle House closures were documented via the Wayback Machine, ",
        "marking one of the few times the chain closed locations en masse—a powerful indicator of the pandemic's severity."
      )
    )
  ),

  # JavaScript for toggle functionality
  tags$script(HTML("
    function toggleInfoPanel() {
      const panel = document.getElementById('info_panel');
      const content = document.getElementById('info_panel_content');
      const icon = document.querySelector('#info_panel_header .toggle-icon');
      panel.classList.toggle('collapsed');
      content.classList.toggle('collapsed');
      icon.classList.toggle('collapsed');
    }
  ")),

  # Clear selection (top-right)
  absolutePanel(
    id = "clear_btn",
    actionButton("clear_sel", "Clear selection",
      icon = icon("times-circle"),
      class = "btn-warning btn-sm"
    )
  ),

  # COVID closures toggle (below clear button)
  absolutePanel(
    id = "covid_btn",
    actionButton("toggle_covid", "Show COVID Closures",
      icon = icon("clock-rotate-left"),
      class = "btn-info btn-sm"
    )
  ),
  conditionalPanel(
    condition = "output.sel_has == 'true'",
    absolutePanel(
      id = "sel_panel",
      div(
        style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;",
        h5("Selected locations", style = "margin:0;"),
        downloadButton("dl_csv", "Export CSV", class = "btn btn-sm btn-outline-secondary")
      ),
      DTOutput("sel_table")
    )
  ),

  # Show small pill only when there is NO selection
  conditionalPanel(
    condition = "output.sel_has == 'false'",
    absolutePanel(
      id = "sel_panel_collapsed",
      span("0 selected")
    )
  )
)



server <- function(input, output, session) {
  # --- data ---
  waffle_sf <- build_waffle_sf()
  if (!"fid" %in% names(waffle_sf)) {
    waffle_sf$fid <- as.character(seq_len(nrow(waffle_sf))) # fallback unique id
  }

  # --- COVID historical data ---
  covid_data <- reactive({
    csv_path <- "historical_data/covid_closures.csv"
    if (file.exists(csv_path)) {
      df <- read.csv(csv_path, stringsAsFactors = FALSE)
      # Add feature ID column
      df$fid <- as.character(seq_len(nrow(df)))
      # Convert to sf object - X is longitude, Y is latitude
      sf::st_as_sf(df, coords = c("X", "Y"), crs = 4326, remove = FALSE)
    } else {
      NULL
    }
  })

  # Track COVID layer visibility
  covid_visible <- reactiveVal(FALSE)

  # --- keep track of selection (by feature ids) ---
  sel_ids <- reactiveVal(character(0))

  # --- map ---
  output$map <- renderMaplibre({
    mt_key <- Sys.getenv("MAPTILER_API_KEY", "")
    style_url <- if (nzchar(mt_key)) {
      paste0("https://api.maptiler.com/maps/streets/style.json?key=", mt_key)
    } else {
      "https://demotiles.maplibre.org/style.json"
    }

    base <- maplibre(
      style  = style_url,
      bounds = waffle_sf,
      pitch  = 45,
      zoom   = 10
    )

    # 3D buildings (use existing source from MapTiler style; unique layer id)
    if (nzchar(mt_key)) {
      base <- base |>
        add_fill_extrusion_layer(
          id = "wh-3d-buildings",
          source = "openmaptiles",
          source_layer = "building",
          fill_extrusion_color = interpolate(
            column = "render_height",
            values = c(0, 150, 300),
            stops  = c("#d9d9d9", "#9ecae1", "#c6dbef")
          ),
          fill_extrusion_height = list(
            "interpolate", list("linear"), list("zoom"),
            15, 0,
            16, list("get", "render_height")
          ),
          fill_extrusion_base = list("get", "render_min_height"),
          fill_extrusion_opacity = 0.9
        )
    }

    # Draw tools (lasso + box)
    base <- base |>
      add_draw_control(
        position = "top-left",
        freehand = TRUE,
        rectangle = TRUE,
        simplify_freehand = TRUE,
        fill_color = "#f59e0b",
        fill_opacity = 0.15,
        line_color = "#f59e0b"
      )

    # Points layer with status-based colors (Green/Yellow/Red for Waffle House Index)
    base |>
      add_circle_layer(
        id = "waffle-points",
        source = waffle_sf,
        circle_color = list(
          "match",
          list("get", "status_simple"),
          "Open (Green)", "#28a745", # Green - Full menu
          "Open 24h", "#28a745", # Green - Always open
          "Closed Temporarily (Red)", "#dc3545", # Red - Closed/severe
          "Open (Yellow)", "#ffc107", # Yellow - Limited menu
          "#ffc107" # Default yellow for unknown
        ),
        circle_radius = 6,
        circle_stroke_color = "#000",
        circle_stroke_width = 1,
        circle_opacity = 0.9,
        tooltip = "tooltip_html"
      )
  })

  # --- respond to drawing: filter map + update selected ids ---
  observeEvent(input$map_drawn_features, ignoreInit = TRUE, {
    proxy <- maplibre_proxy("map")
    drawn <- get_drawn_features(proxy)

    if (is.null(drawn) || nrow(drawn) == 0) {
      if (covid_visible()) {
        set_filter(proxy, "covid-closures", NULL) # show all
      } else {
        set_filter(proxy, "waffle-points", NULL) # show all
      }
      sel_ids(character(0))
      return(invisible())
    }

    poly <- try(sf::st_make_valid(sf::st_union(drawn)), silent = TRUE)
    if (inherits(poly, "try-error")) {
      if (covid_visible()) {
        set_filter(proxy, "covid-closures", NULL)
      } else {
        set_filter(proxy, "waffle-points", NULL)
      }
      sel_ids(character(0))
      return(invisible())
    }

    # Use appropriate data source based on what's visible
    if (covid_visible()) {
      covid_sf <- covid_data()
      if (!is.null(covid_sf)) {
        inside <- sf::st_within(covid_sf, poly, sparse = FALSE)[, 1]
        ids <- covid_sf$fid[inside]

        # apply GL filter to the COVID layer
        if (length(ids) == 0) {
          set_filter(proxy, "covid-closures", list("in", "fid", "")) # show none
        } else {
          set_filter(proxy, "covid-closures", as.list(c("in", "fid", ids)))
        }
        sel_ids(ids)
      }
    } else {
      inside <- sf::st_within(waffle_sf, poly, sparse = FALSE)[, 1]
      ids <- waffle_sf$fid[inside]

      # apply GL filter to the points layer
      if (length(ids) == 0) {
        set_filter(proxy, "waffle-points", list("in", "fid", "")) # show none
      } else {
        set_filter(proxy, "waffle-points", as.list(c("in", "fid", ids)))
      }
      sel_ids(ids)
    }
  })

  # --- clear selection button ---
  observeEvent(input$clear_sel, {
    proxy <- maplibre_proxy("map")
    clear_drawn_features(proxy)
    if (covid_visible()) {
      set_filter(proxy, "covid-closures", NULL)
    } else {
      set_filter(proxy, "waffle-points", NULL)
    }
    sel_ids(character(0))
  })

  # --- toggle COVID layer ---
  observeEvent(input$toggle_covid, {
    proxy <- maplibre_proxy("map")
    covid_sf <- covid_data()

    if (is.null(covid_sf)) {
      showNotification("COVID data file not found", type = "error")
      return()
    }

    if (covid_visible()) {
      # Hide COVID layer and show current locations
      set_layout_property(proxy, "covid-closures", "visibility", "none")
      set_layout_property(proxy, "waffle-points", "visibility", "visible")
      covid_visible(FALSE)
      updateActionButton(session, "toggle_covid",
        label = "Show COVID Closures",
        icon = icon("clock-rotate-left")
      )
    } else {
      # Hide current locations and show COVID layer
      set_layout_property(proxy, "waffle-points", "visibility", "none")

      # Check if layer exists, if not create it
      if (is.null(session$userData$covid_layer_created)) {
        # Create COVID layer with green/red styling
        add_circle_layer(
          proxy,
          id = "covid-closures",
          source = covid_sf,
          circle_color = list(
            "match",
            list("get", "status"),
            "Closed", "#dc3545", # Red for closed
            "#28a745" # Green for open
          ),
          circle_radius = 6,
          circle_opacity = 0.8,
          circle_stroke_color = "#000",
          circle_stroke_width = 1
        )
        session$userData$covid_layer_created <- TRUE
      } else {
        # Show existing layer
        set_layout_property(proxy, "covid-closures", "visibility", "visible")
      }
      covid_visible(TRUE)
      updateActionButton(session, "toggle_covid",
        label = "Hide COVID Closures",
        icon = icon("eye-slash")
      )
    }
  })

  # --- table of selected locations (bottom-right panel) ---
  selected_tbl <- reactive({
    # Check if COVID data is showing
    if (covid_visible()) {
      # Use COVID historical data
      covid_sf <- covid_data()
      if (is.null(covid_sf)) {
        return(tibble::tibble())
      }

      df <- sf::st_drop_geometry(covid_sf)
      ids <- sel_ids()
      if (length(ids) > 0) df <- dplyr::filter(df, fid %in% ids) else df <- df[0, ]

      tibble::tibble(
        Location = paste0("Lat: ", round(df$Y, 4), ", Lon: ", round(df$X, 4)),
        Status = df$status,
        Date = "April 2020"
      )
    } else {
      # Use current waffle house data
      df <- sf::st_drop_geometry(waffle_sf)
      ids <- sel_ids()
      if (length(ids) > 0) df <- dplyr::filter(df, fid %in% ids) else df <- df[0, ]

      href <- ifelse(!is.na(df$online_order) & nzchar(df$online_order),
        df$online_order, df$url
      )
      link <- sprintf("<a href='%s' target='_blank'>Order / Website</a>", href)

      tibble::tibble(
        Name = df$businessName,
        Address = paste0(
          df$addressLines %||% "",
          ifelse(nzchar(df$city %||% ""), paste0(", ", df$city), ""),
          ifelse(nzchar(df$state %||% ""), paste0(", ", df$state), ""),
          " ", df$postalCode %||% ""
        ),
        Status = df$status_simple %||% "Unknown",
        Phone = df$phoneNumbers %||% "",
        Link = link
      )
    }
  })

  output$sel_table <- DT::renderDataTable({
    tbl <- selected_tbl()
    # Determine which columns should escape HTML
    # COVID data has 3 columns (all should escape), current data has 5 (last is link, don't escape)
    escape_cols <- if (covid_visible()) {
      TRUE # All columns for COVID data
    } else {
      c(TRUE, TRUE, TRUE, TRUE, FALSE) # Last column (Link) has HTML for current data
    }

    DT::datatable(
      tbl,
      escape = escape_cols,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 1000,
        scrollY = "34vh",
        deferRender = TRUE,
        ordering = FALSE
      )
    )
  })
  output$sel_has <- renderText({
    if (nrow(selected_tbl()) > 0) "true" else "false"
  })
  outputOptions(output, "sel_has", suspendWhenHidden = FALSE)

  selected_tbl_csv <- reactive({
    # Check if COVID data is showing
    if (covid_visible()) {
      # Use COVID historical data
      covid_sf <- covid_data()
      if (is.null(covid_sf)) {
        return(tibble::tibble())
      }

      df <- sf::st_drop_geometry(covid_sf)
      ids <- sel_ids()
      if (length(ids) > 0) df <- dplyr::filter(df, fid %in% ids) else df <- df[0, ]

      tibble::tibble(
        Latitude = df$Y,
        Longitude = df$X,
        Status = df$status,
        Date = df$ts
      )
    } else {
      # Use current waffle house data
      df <- sf::st_drop_geometry(waffle_sf)
      ids <- sel_ids()
      if (length(ids) > 0) df <- dplyr::filter(df, fid %in% ids) else df <- df[0, ]

      url_out <- ifelse(!is.na(df$online_order) & nzchar(df$online_order),
        df$online_order, df$url
      )

      tibble::tibble(
        Name = df$businessName,
        Address = paste0(
          df$addressLines %||% "",
          ifelse(nzchar(df$city %||% ""), paste0(", ", df$city), ""),
          ifelse(nzchar(df$state %||% ""), paste0(", ", df$state), ""),
          " ", df$postalCode %||% ""
        ),
        Status = df$status_simple %||% "Unknown",
        Phone = df$phoneNumbers %||% "",
        URL = url_out
      )
    }
  })

  # Download selected as CSV
  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0(
        "waffle_selection_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".csv"
      )
    },
    content = function(file) {
      utils::write.csv(selected_tbl_csv(), file, row.names = FALSE, na = "")
    }
  )
}


shinyApp(ui, server)
