# This script is the main script for the shiny app submitted for the midterm
# in BIS620.
# The script sources other scripts in the same folder to build the shiny app.
# One would not source if this were to be deployable but for the sake of the
# midterm we use this method for now. The script has UI and a server functions
# according to the Shiny methodology.
# Authors: Alex Han (alh97), Elisa Loy (eml79), Nokkvi Dan Ellidason (nde4).

# Library and Source would not be done if this was to be deployable. One would
# document according to roxygen2.

library(shiny)
source('vars.R', local = TRUE)
source('utils.R', local = TRUE)
source('ui.R', local = TRUE)
source('server.R', local = TRUE)

# Run the application 
shinyApp(ui = ui, server = server)
