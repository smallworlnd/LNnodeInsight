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

pool <- dbPool(
	drv=PostgreSQL(),
	host=Sys.getenv("DB_HOST"),
	port=5432,
	dbname=Sys.getenv("DB_NAME"),
	user=Sys.getenv("DB_USER"),
	password=Sys.getenv("DB_PW"),
	minSize=1,
	maxSize=3,
	idleTimeout=600000
)

nodes <- pool %>% tbl("nodes_current") %>% as_tibble
links <- pool %>% tbl("edges_current") %>% as_tibble
graph <- as_tbl_graph(links, directed=TRUE) %>%
	rename('id'='name') %>%
	mutate(id=as.numeric(id)) %>%
	left_join(., nodes, by='id') %>%
	activate(edges) %>%
	filter(!is.na(from_fee_rate), from_fee_rate>1) %>%
	activate(nodes) %>%
	mutate(id=row_number())
graph <- graph_insert_chan_balances(pool, graph, 'nd_bal')

pr("api.R") %>%
	pr_hook("exit", function() { poolClose(pool) }) %>%
	pr_run(host="0.0.0.0", port=as.numeric(Sys.getenv("PORT")))
