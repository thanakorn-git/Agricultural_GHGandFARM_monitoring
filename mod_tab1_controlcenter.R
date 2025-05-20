# ─────────────────────────────────────────────────────────────
# mod_tab1_controlcenter.R — CLEAN & RESPONSIVE LAYOUT (v5)
# - Dark mode donut charts: transparent + theme-safe colors
# - Pie backgrounds match bs4Dash themes
# - Eliminated harsh white contrast in dark theme
# ─────────────────────────────────────────────────────────────

library(bs4Dash)
library(leaflet)
library(plotly)
library(sf)
library(dplyr)
library(shinyWidgets)

mod_tab1_control_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 8,
        box(
          title = tagList(icon("leaf"), "Agricultural Monitoring Dashboard"),
          status = "success",
          width = 12,
          solidHeader = TRUE,
          switchInput(ns("live_toggle"), label = "Live Update", size = "mini", onStatus = "success"),
          plotlyOutput(ns("live_chart"), height = "220px"),
          br(),
          fluidRow(
            column(4, plotlyOutput(ns("live_pie1"), height = "140px")),
            column(4, plotlyOutput(ns("live_pie2"), height = "140px")),
            column(4, plotlyOutput(ns("live_pie3"), height = "140px"))
          )
        ),
        box(
          title = "Agricultural Metrics", width = 12, solidHeader = TRUE,
          progressBar(id = ns("p_irrigation"), value = 65, title = tagList(icon("tint"), "Irrigation Level"), status = "success"),
          progressBar(id = ns("p_crop"),       value = 45, title = tagList(icon("seedling"), "Crop Health Index"), status = "info"),
          progressBar(id = ns("p_soil"),       value = 82, title = tagList(icon("water"), "Soil Moisture"), status = "primary"),
          progressBar(id = ns("p_yield"),      value = 33, title = tagList(icon("chart-line"), "Projected Yield Readiness"), status = "danger"),
          br(),
          actionButton(ns("pdf_btn"), "Download Report", icon = icon("file-download"), class = "btn-outline-success"),
          actionButton(ns("bug_btn"), "Flag Issue", icon = icon("exclamation-triangle"), class = "btn-outline-warning")
        )
      ),
      column(
        width = 4,
        box(
          title = "Mini Global View", width = 12, solidHeader = TRUE,
          leafletOutput(ns("mini_map"), height = "220px", width = "100%")
        ),
        box(
          title = "Quick Stats", width = 12, solidHeader = TRUE,
          valueBoxOutput(ns("vb_server"), width = 12),
          valueBoxOutput(ns("vb_disk"),   width = 12),
          valueBoxOutput(ns("vb_temp"),   width = 12)
        )
      )
    ),
    box(
      title = "KPI Summary", width = 12, solidHeader = TRUE,
      fluidRow(
        column(width = 3, valueBoxOutput(ns("vbox_bugs"))),
        column(width = 3, valueBoxOutput(ns("vbox_comments"))),
        column(width = 3, valueBoxOutput(ns("vbox_launches"))),
        column(width = 3, valueBoxOutput(ns("vbox_servers")))
      )
    ),
    box(
      title = "System Metrics", width = 12, solidHeader = TRUE,
      fluidRow(
        column(
          width = 4,
          valueBox("85%", "Disk Utilisation", icon = icon("hard-drive"), color = "orange")
        ),
        column(
          width = 8,
          box(
            title = "Server Temperature Heatmap", width = 12, solidHeader = TRUE,
            plotlyOutput(ns("heatmap"), height = "260px"),
            footer = tagList(icon("fire"), " refreshed every 5 min")
          )
        )
      )
    )
  )
}

mod_tab1_control_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    liveData <- reactiveVal(cumsum(rnorm(50)))
    
    observe({
      invalidateLater(5000, session)
      if (isTRUE(input$live_toggle)) {
        newY <- tail(liveData(), 1) + rnorm(1)
        liveData(c(liveData(), newY)[-1])
      }
    })
    
    output$live_chart <- renderPlotly({
      y <- liveData(); x <- seq_along(y)
      plot_ly() |>
        add_trace(x = x, y = y, type = "scatter", mode = "lines", fill = "tozeroy",
                  line = list(width = 2)) |>
        add_markers(x = tail(x, 1), y = tail(y, 1), marker = list(size = 8)) |>
        layout(margin = list(l = 40, r = 10, t = 10, b = 40), showlegend = FALSE,
               xaxis = list(title = "Time Step", showgrid = FALSE),
               yaxis = list(title = "Signal",   zeroline = TRUE))
    })
    
    # Donut-style pies with transparent backgrounds (dark mode safe)
    renderPieChart <- function(value, label, color) {
      plot_ly(
        values = c(value, 100 - value),
        labels = c("", ""),
        marker = list(colors = c(color, "rgba(255,255,255,0.05)")),
        hole = 0.65,
        type = "pie",
        direction = "clockwise",
        sort = FALSE,
        showlegend = FALSE
      ) |>
        layout(
          annotations = list(
            list(text = paste0(value, "%"), showarrow = FALSE, font = list(size = 18, color = "#ddd"))
          ),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          margin = list(l = 10, r = 10, t = 30, b = 10),
          title = list(text = label, font = list(size = 14, color = "#ccc"), x = 0.5)
        )
    }
    
    output$live_pie1 <- renderPlotly({ renderPieChart(sample(50:100, 1), "Humidity %", "#1ab921") })
    output$live_pie2 <- renderPlotly({ renderPieChart(sample(40:90, 1), "Nitrogen Index", "#11c456") })
    output$live_pie3 <- renderPlotly({ renderPieChart(sample(30:80, 1), "Crop Index", "#53c281") })
    
    output$vbox_bugs     <- renderValueBox(valueBox(21,  "Open Bugs",    icon = icon("bug"),         color = "danger"))
    output$vbox_comments <- renderValueBox(valueBox(143, "Comments",     icon = icon("comment"),     color = "info"))
    output$vbox_launches <- renderValueBox(valueBox(7,   "New Launches", icon = icon("paper-plane"), color = "success"))
    output$vbox_servers  <- renderValueBox(valueBox(4,   "Servers Online",icon = icon("server"),      color = "primary"))
    
    output$vb_server <- renderValueBox(valueBox("75%", "SERVER LOAD", icon = icon("server"),     color = "purple"))
    output$vb_disk   <- renderValueBox(valueBox("79%", "DISK SPACE",  icon = icon("hard-drive"), color = "teal"))
    output$vb_temp   <- renderValueBox(valueBox("36°", "TEMP.",       icon = icon("fire"),       color = "danger"))
    
    output$heatmap <- renderPlotly({
      plot_ly(z = matrix(rnorm(25), 5, 5), type = "heatmap")
    })
    
    world <- sf::st_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", quiet = TRUE) |>
      sf::st_transform(4326)
    
    output$mini_map <- renderLeaflet({
      leaflet(world, options = leafletOptions(minZoom = 1)) |>
        addProviderTiles(providers$CartoDB.DarkMatterNoLabels) |>
        addPolygons(fillColor = "purple", fillOpacity = .4, color = "white", weight = .4) |>
        setView(0, 20, 2)
    })
  })
}
