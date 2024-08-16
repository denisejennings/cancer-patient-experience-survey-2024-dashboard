# -------------------------------------------------------------------------
# CPES Dashboard development
# Denise Jennings, April 2024
# App file
# -------------------------------------------------------------------------

# Source required files to create app
source("scpes_global.R")
source("scpes_dashboard_ui.R")
source("scpes_dashboard_server.R")

shinyApp(ui, server)



