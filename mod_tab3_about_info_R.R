mod_tab3_about_info_UI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "info_tab",
    fluidRow(
      box(
        title = "About This Dashboard",
        width = 12,
        HTML(
          paste0(
            "<p>This application estimates agricultural GHG emissions using Verra VM0042 v2.1.</p>",
            "<p>Sources include fuel combustion, manure deposition, biomass burning, synthetic fertilizer use, and N-fixing species.</p>",
            "<p>It provides a user-friendly interface for data input and visualization.</p>",
            "<p>Users can explore emissions by region, crop type, and management practices.</p>",
            "<p>It also includes a news feed for the latest sustainability news.</p>",
            "<p>Data is sourced from the FAO, IPCC, and other reputable organizations.</p>"
          )
        )
      )
    )
  )
}

mod_tab3_about_info_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Static info page, no server logic yet
  })
}