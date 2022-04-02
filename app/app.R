library(tidyverse)
library(dbplyr)
library(tidygraph)
library(jsonlite)
library(igraph)
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
library(pool)
library(googleCloudRunner)

# load data and functions for applications
source('inst/global.R')

# load user account management
source('inst/session-mgmt.R', local=TRUE)
source('inst/login.R', local=TRUE)
source('inst/logout.R', local=TRUE)
source('inst/accounts-internal.R', local=TRUE)

# pages
source("inst/dashboard.R", local=TRUE)
source("inst/node-stats.R", local=TRUE)
source('inst/byoc.R', local=TRUE)
source('inst/chan-sim.R', local=TRUE)
source('inst/rebal-sim.R', local=TRUE)
source("inst/faq.R", local=TRUE)

# load top-level shiny components
source('inst/server.R', local=TRUE)
source('inst/ui.R', local=TRUE)

onStop(function() {
	cat("\nServer stopped, closing pool\n")
	poolClose(pool)
})
# start the shiny app
shinyApp(ui, server, enableBookmarking = "url")
#cat(file=stderr(), "debug\n")
