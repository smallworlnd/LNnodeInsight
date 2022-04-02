library(tidygraph)
library(tibble)
library(dplyr)
library(stringr)
library(tidyr)
library(igraph)
library(magrittr)
library(plumber)
library(pool)
library(DBI)
library(RPostgreSQL)

source('helpers.R')

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
channels <- pool %>% tbl("edges_current")
balances <- pool %>% tbl("nd_bal") %>% mutate(balance="1M")
chans <- left_join(channels, balances, by=c('from'='from.key', 'to'='to.key')) %>% as_tibble

dir_graph <- as_tbl_graph(chans, directed=TRUE, node_key='pubkey') %>%
	rename('pubkey'='name') %>%
	left_join(., nodes, by='pubkey') %>%
	activate(edges) %>%
	filter(!is.na(from_fee_rate), !is.na(to_fee_rate)) %>%
	activate(nodes) %>%
	mutate(id=row_number())

pr("api.R") %>%
	pr_hook("exit", function() { poolClose(pool) }) %>%
	pr_run(host="0.0.0.0", port=as.numeric(Sys.getenv("PORT")))
