library(tidyverse)
library(tidygraph)
library(dbplyr)
library(jsonlite)
library(igraph)
library(sna)
library(lubridate)
library(intergraph)
library(DBI)
library(RPostgreSQL)

source('store/creds.R')

nodes_agg <- tbl(con, 'nodes')
nodes_agg_3m <- nodes_agg %>% filter(time>=today()-months(3))
nodes_latest <- nodes_agg_3m %>% filter(time>=today()-days(3)) %>% group_by(pubkey) %>% filter(time==last(time)) %>% ungroup

nd_agg <- tbl(con, 'nd')
nd_latest <- nd_agg %>% filter(time>=today()-days(5)) %>% group_by(pubkey) %>% filter(time==last(time)) %>% ungroup

bos_agg <- tbl(con, 'bos')
bos_latest <- bos_agg %>% filter(time>=today()-days(5)) %>% group_by(pubkey) %>% filter(time==last(time)) %>% ungroup %>% window_order(desc(score), pubkey) %>% mutate(rank=rank(-score))

# dbDisconnect(con)
# dbClearResult(dbListResults(con)[[1]])
