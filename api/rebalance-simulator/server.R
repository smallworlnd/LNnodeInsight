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
	port=Sys.getenv("DB_PORT"),
	dbname=Sys.getenv("DB_NAME"),
	user=Sys.getenv("DB_USER"),
	password=Sys.getenv("DB_PW"),
	idleTimeout=600000
)

nodes <- pool %>% tbl("nodes_current") %>% as_tibble
links <- pool %>% tbl("edges_current") %>% as_tibble
dir_graph <- as_tbl_graph(links, directed=TRUE, node_key='pubkey') %>%
	rename('pubkey'='name') %>%
	left_join(., nodes, by='pubkey') %>%
	activate(edges) %>%
	filter(!is.na(from_fee_rate), from_fee_rate>1) %>%
	activate(nodes) %>%
	mutate(id=row_number())
dir_graph <- graph_insert_chan_balances(pool, dir_graph, 'nd_bal')

pr("api.R") %>%
	pr_hook("exit", function() { poolClose(pool) }) %>%
	pr_run(host="0.0.0.0", port=as.numeric(Sys.getenv("PORT")))
