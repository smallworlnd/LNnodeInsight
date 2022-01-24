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
library(DT)
library(stringi)
library(DBI)
library(RPostgreSQL)

# load data and functions for applications
load('data/graph.Rda')

source('inst/db.R', local=TRUE)
source('inst/graph-functions.R', local=TRUE)

# load in invoice management
source('store/api.R')

# load user account management
source('inst/session-mgmt.R', local=TRUE)
source('inst/login.R')
source('inst/logout.R')
source('inst/accounts-internal.R')

# load in build your own chart app
source('inst/byoc.R', local=TRUE)

# load the channel simulator
source('inst/chan-sim.R', local=TRUE)

# load top-level shiny components
source('inst/server.R', local=TRUE)
source('inst/ui.R', local=TRUE)

# start the shiny app
shinyApp(ui, server)
