library(tidygraph)
library(tibble)
library(dplyr)
library(magrittr)
library(plumber)
library(pool)
library(DBI)
library(RPostgreSQL)

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

nodes <- pool %>% tbl("nodes_current") %>% as_tibble

pr("api.R") %>%
	pr_hook("exit", function() { poolClose(pool) }) %>%
	pr_run(host="0.0.0.0", port=as.numeric(Sys.getenv("PORT")))
