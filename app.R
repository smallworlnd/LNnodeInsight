library(tidyverse)
library(dbplyr)
library(tidygraph)
library(jsonlite)
library(igraph)
library(sna)
library(lubridate)
library(intergraph)
library(networkD3)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(htmlwidgets)
library(htmltools)
library(bsplus)
library(rclipboard)
library(httr)
library(plotly)
library(ggVennDiagram)
library(qrencoder)
library(stringi)
library(DBI)
library(RPostgreSQL)

# load data and functions for applications
load('data/graph.Rda')

source('R/db.R', local=TRUE)
source('R/graph-functions.R', local=TRUE)

# load in invoice management
source('store/api.R')

# load user account management
source('R/session-mgmt.R', local=TRUE)
source('R/login.R')
source('R/logout.R')
source('R/accounts-internal.R')

# load in build your own chart app
source('R/byoc.R', local=TRUE)

# load the channel simulator
source('R/chan-sim.R', local=TRUE)

# load top-level shiny components
source('R/server.R', local=TRUE)
source('R/ui.R', local=TRUE)

# start the shiny app
shinyApp(ui, server)
