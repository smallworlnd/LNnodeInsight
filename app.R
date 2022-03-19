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

pool <- dbPool(
        drv=PostgreSQL(),
        host=Sys.getenv("DB_HOST"),
        port=Sys.getenv("DB_PORT"),
        dbname=Sys.getenv("DB_NAME"),
        user=Sys.getenv("DB_USER"),
        password=Sys.getenv("DB_PW")
)

# load data and functions for applications
source('inst/graph-functions.R', local=TRUE)
source('inst/lnni-vars.R')

# load user account management
source('inst/session-mgmt.R', local=TRUE)
source('inst/invoice-mgmt.R', local=TRUE)
source('inst/login.R', local=TRUE)
source('inst/logout.R', local=TRUE)
source('inst/accounts-internal.R', local=TRUE)

# pages
source("inst/lnni-common.R")
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
	poolClose(pool)
})
# start the shiny app
shinyApp(ui, server)
