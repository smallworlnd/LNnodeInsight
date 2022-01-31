library(ghql)
library(jsonlite)
library(magrittr)
library(RPostgreSQL)
library(DBI)
library(tidyverse)

source('store/creds.R')

link <- 'https://api.amboss.space/graphql'
conn <- GraphqlClient$new(url=link)
query <- '
{
  getAllCommunities {
    member_list
    details {
      name
    }
  }
}'
new <- Query$new()$query('link', query)
result <- conn$exec(new$link) %>% fromJSON(flatten=F)

communities <- Map(cbind,
	result$data$getAllCommunities$member_list,
	name=result$data$getAllCommunities$details$name) %>%
		lapply(as.data.frame) %>%
		bind_rows() %>% rename(c('pubkey'='V1', 'community'='name')) %>%
		as_tibble
dbWriteTable(con, 'communities', communities, row.names=FALSE, append=FALSE, overwrite=TRUE)
