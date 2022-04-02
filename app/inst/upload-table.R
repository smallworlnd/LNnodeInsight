library(tidyverse)
library(tidygraph)
library(dbplyr)
library(jsonlite)
library(igraph)
library(lubridate)
library(intergraph)
library(DBI)
library(RPostgreSQL)

source('store/creds.R')

fn <- list.files(path='.', pattern="bos-agg*")
full_table <- lapply(fn, read_tsv) %>% bind_rows
full_table <- full_table %>% mutate(roundtime=round_date(time, unit='days')) %>% arrange(time) %>% distinct(roundtime, pubkey, .keep_all=TRUE) %>% dplyr::select(-roundtime) %>% filter(time>=now()-months(3))

dbWriteTable(con, 'bos', full_table, row.names=FALSE, overwrite=TRUE)
dbGetQuery(con, "CREATE INDEX bos_pubkey_idx ON public.bos USING btree (pubkey)")
dbGetQuery(con, "CREATE INDEX bos_time_idx ON public.bos USING btree (time)")
#dbGetQuery(conn, "CREATE INDEX index_name ON public.db_name USING btree (variable_name)")
dbDisconnect(con)
