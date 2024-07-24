# -------------------------------------------------------------------------
# CPES Dashboard development
# Denise Jennings, April 2024
# App file
# -------------------------------------------------------------------------

# Source required files to create app
source("test_global.R")
source("test_dashboard_ui.R")
source("test_dashboard_server.R")

shinyApp(ui, server)



