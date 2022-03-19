library(tidygraph)
library(tibble)
library(dplyr)
library(tidyr)
library(igraph)
library(magrittr)
library(plumber)
library(pool)
library(DBI)
library(RPostgreSQL)

pool <- dbPool(
	drv=PostgreSQL(),
	host=Sys.getenv("DB_HOST"),
	port=Sys.getenv("DB_PORT"),
	dbname=Sys.getenv("DB_NAME"),
	user=Sys.getenv("DB_USER"),
	password=Sys.getenv("DB_PW"),
	minSize=1,
	maxSize=3,
	idleTimeout=600000
)
#poolClose(pool)

pr("api.R") %>%
	pr_hook("exit", function() { poolClose(pool) }) %>%
	pr_run(host="0.0.0.0", port=as.numeric(Sys.getenv("PORT")))
