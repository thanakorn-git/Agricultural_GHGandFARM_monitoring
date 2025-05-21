# app.R ── FINAL RESPONSIVE VERSION ────────────────────────────────

# Load core packages ----------------------------------------------------------
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(shinyalert)

# -----------------------------------------------------------------------------
# Source modules & helpers ----------------------------------------------------
source("mod_tab1_controlcenter.R")
source("mod_tab2_calculator_R.R", local = TRUE)
source("mod_tab3_about_info_R.R", local = TRUE)
source("helpers_calc_ghg_vm0042_R.R", local = TRUE)
source("helpers_colors_theme_R.R", local = TRUE)

# -----------------------------------------------------------------------------
# UserInterface ----------------------------------------------------------------
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  title = "GHG Control Center",
  
  # HEADER ---------------------------------------------------------------------
  header = dashboardHeader(),
  
  # SIDEBAR --------------------------------------------------------------------
  sidebar = dashboardSidebar(
    skin = "dark",
    collapsed = FALSE,
    minified = TRUE,
    
    # ✅ Logo above menu
    tags$div(
      style = "text-align:center; padding:10px;",
      tags$img(
        src = "https://swjiwyhenhllhkositzx.supabase.co/storage/v1/object/public/product/test/faa_logo.png",
        id = "sidebar-logo",
        style = "width:70%; max-width:160px; transition:all 0.3s;"
      )
    ),
    
    sidebarMenu(
      menuItem("World Map Overview", tabName = "map_tab",  icon = icon("globe", verify_fa = FALSE)),
      menuItem("GHG Calculator",      tabName = "calc_tab", icon = icon("calculator", verify_fa = FALSE)),
      menuItem("About & Methodology", tabName = "info_tab", icon = icon("circle-info", verify_fa = FALSE))
    )
  ),
  
  # BODY -----------------------------------------------------------------------
  body = dashboardBody(
    useShinyjs(),
    useShinyalert(force = TRUE),
    
    # Custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "smartadmin.css"),
      tags$style(HTML("
        /* Responsive logo resizing for collapsed sidebar */
        .sidebar-mini.sidebar-collapse #sidebar-logo {
          width: 30px !important;
          max-width: 30px !important;
          transition: all 0.3s ease-in-out;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "map_tab",  mod_tab1_control_UI("cc")),
      tabItem(tabName = "calc_tab", mod_tab2_calculator_UI("calc")),
      tabItem(tabName = "info_tab", mod_tab3_about_info_UI("info"))
    )
  ),
  
  # CONTROLBAR & FOOTER -------------------------------------------------------
  controlbar = dashboardControlbar(disable = TRUE),
  footer     = dashboardFooter(
    left  = "© 2025 Fairagora Asia",
    right = "Powered by VM0042"
  )
)

# -----------------------------------------------------------------------------
# Serverlogic ------------------------------------------------------------------
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  mod_tab1_control_Server("cc")
  mod_tab2_calculator_Server("calc")
  mod_tab3_about_info_Server("info")
}

# -----------------------------------------------------------------------------
# Run the app ------------------------------------------------------------------
# -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
