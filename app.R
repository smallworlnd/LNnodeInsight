library(tidyverse)
library(tidygraph)
library(jsonlite)
library(igraph)
library(sna)
library(lubridate)
library(intergraph)
library(networkD3)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(htmlwidgets)
library(htmltools)
library(bsplus)
library(rclipboard)
library(plotly)
library(ggVennDiagram)
library(qrencoder)
library(DT)

# load data and functions for applications
#source('inst/data.R')
load('graph.Rda')
source('inst/functions.R', local=TRUE)

# load in invoice management
source('store/api.R')

# load in shiny parts
source('inst/server.R', local=TRUE)
source('inst/ui.R', local=TRUE)

# put it all together
shinyApp(ui, server)
