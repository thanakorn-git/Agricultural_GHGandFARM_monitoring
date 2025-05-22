# ─────────────────────────────────────────────────────────────
# mod_tab2_calculator_R.R  — v8  (classic vertical layout, dropdown bottom,
#                                 map default view + auto-zoom on confirm)
# ─────────────────────────────────────────────────────────────
library(bs4Dash)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(dplyr)
library(sf)
library(DT)
library(shinyjs)
library(shinyalert)

# ---------- MOCK farmer polygons -------------------------------------------
mock_meta <- data.frame(
  farmer_id   = sprintf("%03d", 1:5),
  farm_id     = paste0("F0", 1:5),
  farmer_name = c("Mr. Somchai", "Ms. Duang", "Mr. Anon", "Ms. Rung", "Mr. Kiet"),
  stringsAsFactors = FALSE
)

poly_list <- list(
  matrix(c(98.95,18.77, 98.96,18.77, 98.96,18.78, 98.95,18.78, 98.95,18.77), ncol=2, byrow=TRUE),
  matrix(c(102.83,16.44,102.84,16.44,102.84,16.45,102.83,16.45,102.83,16.44), ncol=2, byrow=TRUE),
  matrix(c(100.48,13.72,100.49,13.72,100.49,13.73,100.48,13.73,100.48,13.72), ncol=2, byrow=TRUE),
  matrix(c(99.31, 9.12, 99.32, 9.12, 99.32, 9.13, 99.31, 9.13, 99.31, 9.12),  ncol=2, byrow=TRUE),
  matrix(c(105.42,15.25,105.43,15.25,105.43,15.26,105.42,15.26,105.42,15.25), ncol=2, byrow=TRUE)
)

wkt_vec <- vapply(poly_list,
                  \(m) sf::st_as_text(sf::st_polygon(list(m))),
                  character(1))

mock_sf <- mock_meta |>
  mutate(wkt = wkt_vec) |>
  sf::st_as_sf(wkt = "wkt", crs = 4326)

# ---------- UI --------------------------------------------------------------
mod_tab2_calculator_UI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "calc_tab",
    useShinyjs(),
    
    # Row 1 : Farmer record (full width)
    fluidRow(
      box(
        title = "Farmer Record", status = "warning", solidHeader = TRUE, width = 12,
        textInput(ns("farmer_id"), "Farmer ID"),
        textInput(ns("farm_id"),   "Farm ID"),
        numericInput(ns("farm_year"), "Year", value = 2025, min = 2000, max = 2100),
        textAreaInput(ns("farm_notes"), "Notes", rows = 2),
        
        # dropdown now at bottom
        pickerInput(ns("meta_quick"), "Select Mock Record",
                    choices = paste(mock_meta$farmer_id, mock_meta$farm_id),
                    options = list(`live-search` = TRUE)),
        
        actionButton(ns("confirm_meta"), "Confirm Record", class = "btn-primary")
      )
    ),
    
    # Row 2 : Map (full width)
    fluidRow(
      box(
        title = "Farm Location", status = "success", solidHeader = TRUE, width = 12,
        leafletOutput(ns("farm_map"), height = 400)
      )
    ),
    
    # Row 3 : Input boxes side-by-side
    fluidRow(
      box(
        title = "Fuel and Biomass Inputs", status = "primary",
        solidHeader = TRUE, width = 6,
        selectInput(ns("fuel_type"), "Fuel Type:", choices = c("Diesel", "Gasoline", "LPG")),
        numericInput(ns("fuel_amount"), "Fuel Volume (liters):", value = 0, min = 0),
        numericInput(ns("sugarcane_yield"), "Sugarcane Yield (t/ha):", value = 0, min = 0),
        sliderInput(ns("burn_fraction"), "Fraction of Trash Burned:", min = 0, max = 1, step = 0.1, value = 1),
        sliderInput(ns("dry_matter"), "Dry Matter Content (0.4‒0.6)",
                    min = 0.4, max = 0.6, step = 0.01, value = 0.45)
      ),
      box(
        title = "Livestock and Fertilizer Inputs", status = "primary",
        solidHeader = TRUE, width = 6,
        selectInput(ns("animal_type"), "Animal Type:", choices = c("Cattle", "Buffalo", "Goat")),
        numericInput(ns("animal_number"), "Number of Animals:", value = 0, min = 0),
        numericInput(ns("animal_weight"), "Average Animal Weight (kg):", value = 500, min = 0),
        numericInput(ns("fert_mass"), "Fertilizer Applied (kg/ha):", value = 0, min = 0),
        numericInput(ns("fert_n_pct"), "Fertilizer N Content (%):", value = 10, min = 0, max = 100),
        actionButton(ns("calculate"), "Calculate GHG Emissions", class = "btn-success"),
        actionButton(ns("reset"), "Reset All Inputs", class = "btn-danger")
      )
    ),
    
    # Row 4 : Output
    fluidRow(
      box(
        title = "Emissions Output", status = "info", solidHeader = TRUE, width = 12,
        htmlOutput(ns("result_display")),
        plotlyOutput(ns("emissions_graph"))
      )
    ),
    
    # Row 5 : Records table
    fluidRow(
      box(
        title = "Saved Emission Records", status = "secondary", solidHeader = TRUE, width = 12,
        DTOutput(ns("records_table"))
      )
    )
  )
}

# ---------- Server ----------------------------------------------------------
mod_tab2_calculator_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    rv <- reactiveValues(
      meta    = NULL,
      polys   = mock_sf[0, ],   # empty sf
      records = NULL
    )
    
    # auto-fill picker
    observeEvent(input$meta_quick, {
      parts <- strsplit(input$meta_quick, " ")[[1]]
      updateTextInput(session, "farmer_id", value = parts[1])
      updateTextInput(session, "farm_id",   value = parts[2])
    })
    
    # confirm meta
    observeEvent(input$confirm_meta, {
      req(input$farmer_id, input$farm_id)
      rv$meta <- list(
        farmer_id = input$farmer_id,
        farm_id   = input$farm_id,
        year      = input$farm_year,
        notes     = input$farm_notes
      )
      shinyalert("Meta saved!", type = "info", timer = 1500, showConfirmButton = FALSE)
    })
    
    # load polygon & zoom
    observeEvent(rv$meta, {
      sel <- mock_sf |> filter(farmer_id == rv$meta$farmer_id,
                               farm_id   == rv$meta$farm_id)
      if (nrow(sel) == 1) {
        rv$polys <- sel
        
        bb <- sf::st_bbox(sel)
        leafletProxy(ns("farm_map")) |>
          clearShapes() |>
          addPolygons(data = sel,
                      layerId = sel$farm_id,
                      fillColor = "#2ecc71", fillOpacity = 0.55,
                      color = "#1e8449", weight = 3,
                      popup = paste0("<b>Farmer </b>", sel$farmer_id)) |>
          flyToBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
      } else {
        shinyalert("No polygon for this ID!", type = "error")
      }
    })
    
    # initial base map (satellite default)
    output$farm_map <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
        addProviderTiles(providers$CartoDB.Positron,   group = "Positron")  |>
        setView(lng = 100, lat = 13.5, zoom = 5) |>
        addLayersControl(baseGroups = c("Satellite", "Positron"),
                         options = layersControlOptions(collapsed = FALSE))
    })
    
    # ─── calculation / output / records (unchanged) ────────────────────────
    results <- reactiveVal(NULL)
    
    observeEvent(input$calculate, {
      req(rv$meta)
      res <- calc_total_ghg(
        fuel_type       = input$fuel_type,
        fuel_amount     = input$fuel_amount,
        animal_type     = input$animal_type,
        animal_count    = input$animal_number,
        animal_weight   = input$animal_weight,
        sugarcane_yield = input$sugarcane_yield,
        burn_fraction   = input$burn_fraction,
        dry_matter      = input$dry_matter,
        fert_mass       = input$fert_mass,
        fert_n_pct      = input$fert_n_pct
      )
      results(res)
      
      rv$records <- bind_rows(
        rv$records,
        data.frame(
          farmer_id       = rv$meta$farmer_id,
          farm_id         = rv$meta$farm_id,
          year            = rv$meta$year,
          total_emissions = res$TOTAL,
          notes           = rv$meta$notes,
          stringsAsFactors = FALSE
        )
      )
      
      shinyalert("Calculation Complete!", type = "success",
                 timer = 2500, showConfirmButton = FALSE)
    })
    
    # reset (unchanged)
    observeEvent(input$reset, {
      updateSelectInput(session, "fuel_type", selected = "Diesel")
      updateNumericInput(session, "fuel_amount", value = 0)
      updateNumericInput(session, "sugarcane_yield", value = 0)
      updateSliderInput(session, "burn_fraction", value = 1)
      updateSliderInput(session, "dry_matter", value = 0.45)
      updateSelectInput(session, "animal_type", selected = "Cattle")
      updateNumericInput(session, "animal_number", value = 0)
      updateNumericInput(session, "animal_weight", value = 500)
      updateNumericInput(session, "fert_mass", value = 0)
      updateNumericInput(session, "fert_n_pct", value = 10)
      results(NULL)
    })
    
    output$result_display <- renderUI({
      req(results())
      total <- results()$TOTAL
      col <- if (total > 10) "red" else if (total > 5) "orange" else "green"
      HTML(sprintf("<h3 style='color:%s;'>Total Emissions: %.2f tCO₂e/ha</h3>", col, total))
    })
    
    output$emissions_graph <- renderPlotly({
      req(results())
      df <- data.frame(Gas = c("CO₂", "CH₄", "N₂O"),
                       Emission = c(results()$CO2,
                                    results()$CH4,
                                    results()$N2O))
      plot_ly(df, x = ~Gas, y = ~Emission, type = "bar",
              color = ~Gas, colors = c("steelblue", "orange", "darkred"),
              text = ~paste0(round(Emission, 2), " tCO₂e"), hoverinfo = "text") |>
        layout(title = "GHG Emissions by Gas Type",
               yaxis = list(title = "tCO₂e/ha"),
               xaxis = list(title = "Gas"))
    })
    
    output$records_table <- renderDT({
      req(rv$records)
      datatable(rv$records, options = list(pageLength = 5))
    })
  })
}
