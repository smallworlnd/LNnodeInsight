library(tibble)
library(dplyr)
library(magrittr)
library(lubridate)
library(plumber)
library(httr)
library(pool)
library(DBI)
library(RPostgreSQL)
library(stringi)
library(sodium)

source("./data-price-functions.R")
source("./utils.R")

tryCatch({
	pool <- dbPool(
		drv=PostgreSQL(),
		host=Sys.getenv("DB_HOST"),
		port=Sys.getenv("DB_PORT"),
		dbname=Sys.getenv("DB_NAME"),
		user=Sys.getenv("DB_USER"),
		password=Sys.getenv("DB_PW"))
	},
	error = function(e) { 
		print(e)
		stop("Could not make connection to db")
	},
	warning = function(w) {
		print(w)
	}
)

pr() %>%
	pr_set_serializer(serializer_unboxed_json(pretty=TRUE)) %>%
	pr_mount("/v1/sats4stats", plumb("sats4stats-submit-api.R")) %>%
	pr_mount("/v1/stats", plumb("summary-stats-api.R")) %>%
	pr_hook("exit", function() {poolClose(pool)}) %>%
	pr_run(host="0.0.0.0", port=as.numeric(Sys.getenv("PORT")))
