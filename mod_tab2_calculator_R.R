# Enhanced mod_tab2_calculator_R.R

mod_tab2_calculator_UI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "calc_tab",
    fluidRow(
      box(
        title = "Fuel and Biomass Inputs", status = "primary", solidHeader = TRUE, width = 6,
        selectInput(ns("fuel_type"), "Fuel Type:", choices = c("Diesel", "Gasoline", "LPG")),
        numericInput(ns("fuel_amount"), "Fuel Volume (liters):", value = 0, min = 0),
        numericInput(ns("sugarcane_yield"), "Sugarcane Yield (t/ha):", value = 0, min = 0),
        sliderInput(ns("burn_fraction"), "Fraction of Trash Burned:", min = 0, max = 1, step = 0.1, value = 1),
        sliderInput(ns("dry_matter"), "Dry Matter Content (range 0.4-0.6)", min = 0.4, max = 0.6, step = 0.01, value = 0.45)
      ),
      box(
        title = "Livestock and Fertilizer Inputs", status = "primary", solidHeader = TRUE, width = 6,
        selectInput(ns("animal_type"), "Animal Type:", choices = c("Cattle", "Buffalo", "Goat")),
        numericInput(ns("animal_number"), "Number of Animals:", value = 0, min = 0),
        numericInput(ns("animal_weight"), "Average Animal Weight (kg):", value = 500, min = 0),
        numericInput(ns("fert_mass"), "Fertilizer Applied (kg/ha):", value = 0, min = 0),
        numericInput(ns("fert_n_pct"), "Fertilizer N Content (%):", value = 10, min = 0, max = 100),
        actionButton(ns("calculate"), "Calculate GHG Emissions", class = "btn-success"),
        actionButton(ns("reset"), "Reset All Inputs", class = "btn-danger")
      )
    ),
    fluidRow(
      box(
        title = "Emissions Output", status = "info", solidHeader = TRUE, width = 12,
        htmlOutput(ns("result_display")),
        plotlyOutput(ns("emissions_graph"))
      )
    )
  )
}

mod_tab2_calculator_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    results <- reactiveVal(NULL)
    
    observeEvent(input$calculate, {
      result <- calc_total_ghg(
        fuel_type = input$fuel_type,
        fuel_amount = input$fuel_amount,
        animal_type = input$animal_type,
        animal_count = input$animal_number,
        animal_weight = input$animal_weight,
        sugarcane_yield = input$sugarcane_yield,
        burn_fraction = input$burn_fraction,
        dry_matter = input$dry_matter,
        fert_mass = input$fert_mass,
        fert_n_pct = input$fert_n_pct
      )
      results(result)
      
      shinyalert::shinyalert(
        title = "Calculation Complete!",
        text = "Your GHG emissions results have been updated successfully.",
        type = "success",
        timer = 2500,
        showConfirmButton = FALSE
      )
    })
    
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
      color <- if (total > 10) "red" else if (total > 5) "orange" else "green"
      HTML(sprintf("<h3 style='color:%s;'>Total Emissions: %.2f tCO₂e/ha</h3>", color, total))
    })
    
    output$emissions_graph <- renderPlotly({
      req(results())
      df <- data.frame(
        Gas = c("CO₂", "CH₄", "N₂O"),
        Emission = c(results()$CO2, results()$CH4, results()$N2O)
      )
      
      plot_ly(
        data = df,
        x = ~Gas,
        y = ~Emission,
        type = 'bar',
        color = ~Gas,
        colors = c("steelblue", "orange", "darkred"),
        text = ~paste0(round(Emission, 2), " tCO₂e"),
        hoverinfo = "text"
      ) %>%
        layout(
          title = "GHG Emissions by Gas Type",
          yaxis = list(title = "tCO₂e/ha"),
          xaxis = list(title = "Gas")
        )
    })
  })
}
