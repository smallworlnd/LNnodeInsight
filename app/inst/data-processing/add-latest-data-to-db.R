# requires the .Renviron file be configured as described in Renviron.template

library(tidyverse)
library(tidygraph)
library(tidyr)
library(jsonlite)
library(igraph)
library(lubridate)
library(intergraph)
library(httr)
library(DBI)
library(RPostgreSQL)
library(ghql)

build_graph_from_data_req <- function(data_req) {
	if ("date" %in% names(data_req)) {
		# fetch screenshot time of the graph
		ss.time <- as_datetime(data_req$date, tz="UTC")
		# pull graph content from request
		dg_resp <- content(data_req, as="text") %>% fromJSON(flatten=TRUE)
	} else {
		ss.time <- as_datetime(as.numeric(str_split(data_req, "[-.]")[[1]][3]), tz="UTC")
		dg_resp <- fromJSON(data_req, flatten=TRUE)
	}

	# fetch nodes
	nodes <- dg_resp$nodes %>%
		unnest(addresses) %>%
		dplyr::select(pub_key, alias, network, addr) %>%
		mutate(network=
			ifelse(grepl("^[0-9]+.[0-9]+.[0-9]+.[0-9]+:[0-9]+$", addr), "ipv4",
			ifelse(grepl("^[a-z0-9]+.onion:[0-9]+$", addr), "torv3",
			ifelse(grep("^\\[[a-z0-9]+:[a-z0-9]+:[a-z0-9]+:[a-z0-9]+:[a-z0-9]+:[a-z0-9]+:[a-z0-9]+:[a-z0-9]+\\]:[0-9]+$", addr), "ipv6", NA)))) %>%
			unique %>%
			pivot_wider(names_from=network, values_from=addr) %>%
			unnest(cols=c(ipv4, ipv6, torv3)) %>%
			rename('pubkey'='pub_key') %>%
			distinct(pubkey, .keep_all=TRUE)
	# build graph
	channels <- dg_resp$edges %>%
		dplyr::select(node1_pub, node2_pub, capacity, last_update, node1_policy.fee_base_msat, node1_policy.fee_rate_milli_msat, node2_policy.fee_base_msat, node2_policy.fee_rate_milli_msat) %>%
		rename(c('from_base_fee'='node1_policy.fee_base_msat', 'from_fee_rate'='node1_policy.fee_rate_milli_msat', 'to_base_fee'='node2_policy.fee_base_msat', 'to_fee_rate'='node2_policy.fee_rate_milli_msat', 'from'='node1_pub', 'to'='node2_pub')) %>%
		mutate_at(vars(capacity, from_base_fee, from_fee_rate, to_base_fee, to_fee_rate), as.numeric) %>%
		as_tibble

	# expand single edges into 2 to express asymmetric chann properties
	rev_edges <- channels %>%
		mutate(f=to, t=from, fbf=to_base_fee, tbf=from_base_fee, ff=to_fee_rate, tf=from_fee_rate, direction=0) %>%
		dplyr::select(f, t, capacity, last_update, fbf, ff, tbf, tf, direction) %>%
		rename(c('from'='f', 'to'='t', 'from_base_fee'='fbf', 'from_fee_rate'='ff', 'to_base_fee'='tbf', 'to_fee_rate'='tf'))
	forw_edges <- channels %>% mutate(direction=1)
	all_edges <- rbind(forw_edges, rev_edges) %>%
		mutate(time=ss.time, day=floor_date(ss.time, 'day'))
	capacity <- all_edges %>%
		group_by(from) %>%
		summarise(
			tot.capacity=sum(capacity, na.rm=TRUE),
			num.channels=n(),
			avg.capacity=mean(capacity, na.rm=TRUE),
			med.capacity=median(capacity, na.rm=TRUE)) %>%
		rename('pubkey'='from')
	chanstates <- all_edges %>% mutate(diff=as.numeric(day-as_datetime(last_update, tz="UTC"), unit="days"), state=ifelse(diff>14, "inact.channels", "act.channels")) %>%
		dplyr::select(from, state) %>%
		group_by(from, state) %>%
		summarise(n=n()) %>%
		rename('pubkey'='from') %>%
		pivot_wider(names_from=state, values_from=n, values_fill=0) %>%
		ungroup
	fees <- all_edges %>%
		group_by(from) %>%
		summarise(
			mean.base.msat.out=mean(from_base_fee, na.rm=TRUE),
			mean.rate.ppm.out=mean(from_fee_rate, na.rm=TRUE),
			median.base.msat.out=median(from_base_fee, na.rm=TRUE),
			median.rate.ppm.out=median(from_fee_rate, na.rm=TRUE),
			mean.base.msat.in=mean(to_base_fee, na.rm=TRUE),
			mean.rate.ppm.in=mean(to_fee_rate, na.rm=TRUE),
			median.base.msat.in=median(to_base_fee, na.rm=TRUE),
			median.rate.ppm.in=median(to_fee_rate, na.rm=TRUE)) %>%
		rename('pubkey'='from')

	## redacted
	g <- as_tbl_graph(all_edges, directed=TRUE, node_key='pubkey') %>%
		rename('pubkey'='name') %>%
		left_join(., capacity, by='pubkey') %>%
		left_join(., fees, by='pubkey') %>%
		left_join(., chanstates, by='pubkey') %>%
		left_join(., nodes, by='pubkey')
	# keep only the main network
	g <- decompose(g, mode='weak')[[1]] %>%
		as_tbl_graph %>%
		mutate(id=row_number())

	# heuristics to speed up centrality measures
	# ignore nodes with >50% inactive channels, total capacity <1e5 (q1) and only 1 channel (q1)
	heuristics <- g %>%
		as_tibble %>%
		dplyr::select(tot.capacity, num.channels) %>%
		summarise(q1capacity=quantile(tot.capacity, 0.25, na.rm=TRUE), q1num.channels=quantile(num.channels, 0.25, na.rm=TRUE))
	g_heur <- g %>%
		filter(
			act.channels>0,
			tot.capacity>heuristics$q1capacity,
			num.channels>heuristics$q1num.channels) %>%
		activate(edges) %>%
		filter(direction==0) %>%
		activate(nodes)
	g_heur <- decompose(g_heur, mode='weak')[[1]] %>%
		as_tbl_graph %>%
		mutate(id=row_number())
	g_heur_pubkeys <- g_heur %>%
		as_tibble %>%
		pull(pubkey)
	g_betw <- centr_betw(g_heur, directed=FALSE)
	g_clo <- centr_clo(g_heur, mode='all')
	g_eigen <- centr_eigen(g_heur, directed=FALSE)
	# capacity weighted betweenness
	w <- g_heur %>% activate(edges) %>% pull(capacity)
	w <- 1/(w/1e8)
	g_betw_w <- igraph::betweenness(g_heur, directed=FALSE, weights=w) %>%
		enframe %>%
		rename(c('id'='name', 'cent.between.weight'='value'))
	g_clo_w <- igraph::closeness(g_heur, weights=w) %>%
		enframe %>%
		rename(c('id'='name', 'cent.close.weight'='value'))
	g_eigen_w <- igraph::eigen_centrality(g_heur, directed=FALSE, weights=1/w)$vector %>%
		enframe %>%
		rename(c('id'='name', 'cent.eigen.weight'='value'))

	g_cent_summ <- tibble(
		pubkey=g_heur_pubkeys,
		cent.between=g_betw$res,
		cent.close=g_clo$res,
		cent.eigen=g_eigen$vector)
	g_cent_w_summ <- tibble(
		pubkey=g_heur_pubkeys,
		cent.between.weight=g_betw_w$cent.between.weight,
		cent.close.weight=g_clo_w$cent.close.weight,
		cent.eigen.weight=g_eigen_w$cent.eigen.weight)

	nodes <- list(g, g_cent_summ, g_cent_w_summ) %>% 
		reduce(left_join, by="pubkey") %>%
		as_tibble %>%
		mutate(
			time=ss.time,
			cent.between.rank=rank(-cent.between, ties.method='first'),
			cent.eigen.rank=rank(-cent.eigen, ties.method='first'),
			cent.close.rank=rank(-cent.close, ties.method='first'),
			cent.between.weight.rank=rank(-cent.between.weight, ties.method='first'),
			cent.close.weight.rank=rank(-cent.close.weight, ties.method='first'),
			cent.eigen.weight.rank=rank(-cent.eigen.weight, ties.method='first'))
	return(list(nodes=nodes, channels=all_edges))
}

summarise_nd_from_data_req <- function(data_req) {
	ss.time <- now("UTC")
	nd_resp <- content(data_req, as="text") %>% fromJSON(flatten=TRUE)

	chan_states <- lapply(
		c('scored', 'stable'),
		function(x)
			eval(parse(text=paste0('nd_resp$', x))) %>%
			unlist %>%
			as.data.frame %>%
			rownames_to_column %>%
			separate(rowname, into=c('name', 'key'), extra='merge') %>%
			mutate(
				key=str_replace(key, "peers?[0-9]*", "peer"),
				key=str_replace(key, "addresses?[0-9]*", "address"),
				key=str_remove_all(key, '(good_|stable_|_peer)')) %>%
			as_tibble %>%
			unique %>%
			rename('value'='.') %>%
			group_by(name) %>%
			do(add_row(., key="state", value=x)) %>%
			ungroup %>%
			fill(name)
		) %>%
		bind_rows
	nd_scored_stable <- chan_states %>%
		group_by(name, value) %>%
		mutate(t1=n()) %>%
		ungroup %>%
		mutate(key=ifelse((key=="inbound" | key=="outbound") & t1==2, "inbound_and_outbound", key)) %>%
		unique %>%
		group_by(name, key) %>%
		mutate(t1=n()) %>%
		ungroup %>%
		mutate(value=ifelse(key=="inbound_and_outbound" | key=="inbound" | key=="outbound", t1, value)) %>%
		unique %>%
		dplyr::select(-t1)
	nd_unstable <- nd_resp$unstable %>%
		unlist %>%
		as.data.frame %>%
		rownames_to_column %>%
		separate(rowname, into=c('name', 'key'), extra='merge') %>%
		mutate(key=str_replace(key, "peers?[0-9]*", "peer"), key=str_replace(key, "addresses?[0-9]*", "address")) %>%
		as_tibble %>%
		rename('value'='.') %>%
		mutate(state=ifelse(value=="TRUE", key, NA)) %>%
		mutate(key=ifelse(value=="TRUE", "state", key), value=ifelse(value=="TRUE", state, value)) %>%
		dplyr::select(-state)
	nd_agg <- rbind(nd_scored_stable, nd_unstable) %>%
		filter(key=='score' | key=='centrality' | key=='inbound_and_outbound' | key=="inbound" | key=="outbound" | key=="state") %>%
		pivot_wider(names_from=key, values_from=value) %>%
		mutate(score=as.numeric(score), centrality=as.numeric(centrality), inbound=as.numeric(inbound), outbound=as.numeric(outbound), inbound_and_outbound=as.numeric(inbound_and_outbound)) %>%
		arrange(desc(score), desc(centrality)) %>%
		replace_na(list(inbound=0, outbound=0, inbound_and_outbound=0)) %>%
		mutate(rank=rank(-score, ties.method='first'), time=ss.time, good_peers=inbound+outbound+inbound_and_outbound) %>%
		rename('pubkey'='name')
	high_bal <- chan_states %>%
		filter(key=='inbound' | key=='outbound') %>%
		mutate(
			from.key=ifelse(key=='inbound', value, name),
			to.key=ifelse(key=='outbound', value, name)) %>%
		dplyr::select(from.key, to.key)
	fail_state <- nd_unstable %>% filter(key=='state') %>% dplyr::select(-key)
	return(list(agg=nd_agg, bal=high_bal, state=fail_state))

}

summarise_amboss_communities_from_data_req <- function(data_req) {
	communities <- Map(cbind,
		data_req$data$getAllCommunities$member_list,
		name=data_req$data$getAllCommunities$details$name) %>%
			lapply(as.data.frame) %>%
			bind_rows() %>% rename(c('pubkey'='V1', 'community'='name')) %>%
			as_tibble
	return(communities)
}

# connect to the db
tryCatch({
	con <- dbConnect(PostgreSQL(),    
		host=Sys.getenv("DB_HOST"),
		port=Sys.getenv("DB_PORT"),
		dbname=Sys.getenv("DB_NAME"),
		user=Sys.getenv("DB_USER"),
		password=Sys.getenv("DB_PW"))
	},
	error = function(e) { 
		print(e)
		stop("Cannot proceed without a DB connection. Fail fast to avoid uncessary computations.")
	},
	warning = function(w) {
		print('warning db open')
		print(w)
	}
)

if (length(commandArgs(trailingOnly=TRUE)) > 0) {
	tryCatch({
		latest_graph <- build_graph_from_data_req(commandArgs(trailingOnly=TRUE)[1])
		# split the graph into parts for easy uploading
		latest_node_summary <- latest_graph$nodes
		latest_edges <- latest_graph$channels
		graph_error <- http_error(dg_req)
		},
		error = function(e) { 
			print(e)
			print("Could not fetch graph data")
			graph_error <- http_error(dg_req)
		}
	)
} else {
	tryCatch({
		graph_headers <- add_headers(c("Grpc-Metadata-macaroon"=Sys.getenv("GRAPH_MACAROON")))
		dg_req <- GET(url=Sys.getenv("GRAPH_ENDPOINT"), config=graph_headers)
		latest_graph <- build_graph_from_data_req(dg_req)
		# split the graph into parts for easy uploading
		latest_node_summary <- latest_graph$nodes
		latest_edges <- latest_graph$channels
		graph_error <- http_error(dg_req)
		},
		error = function(e) { 
			print(e)
			print("Could not fetch graph data")
			graph_error <- http_error(dg_req)
		}
	)
}

# upload latest graph data to the db
if (!graph_error) {
	tryCatch({
		dbWriteTable(con, 'nodes_historical', latest_graph$nodes %>% dplyr::select(-c(ipv4, ipv6, torv3)), row.names=FALSE, overwrite=FALSE, append=TRUE)
		dbExecute(con, 'delete from nodes_historical where "time" <= now() - interval \'3 month\'')
		dbWriteTable(con, 'nodes_current', latest_graph$nodes, row.names=FALSE, overwrite=TRUE)
		dbWriteTable(con, 'edges_current', latest_graph$channels, row.names=FALSE, overwrite=TRUE)
		print("Wrote graph data to db")

		print("Fetching new users")
		users_in_db <- tbl(con, 'users') %>% collect %>% dplyr::select(pubkey, alias) %>% unique
		users_in_graph <- latest_node_summary %>% dplyr::select(pubkey, alias)
		new_users <- setdiff(users_in_graph$pubkey, users_in_db$pubkey)
		print(paste("Have", length(new_users), "new users to add"))
		if (length(new_users) > 0) {
			users_to_add <- users_in_graph %>% filter(pubkey %in% new_users) %>% dplyr::select(pubkey, alias)
			subscription <- 'Standard'
			sub_date <- now("UTC")
			sub_expiration_date <- NA
			new_users_df <- data.frame(users_to_add, subscription, sub_date, sub_expiration_date)
			names(new_users_df) <- c('pubkey', 'alias', 'subscription', 'sub_date', 'sub_expiration_date')
			dbWriteTable(con, 'users', new_users_df, row.names=FALSE, overwrite=FALSE, append=TRUE)
			print(paste("Added", length(new_users), "new users"))
		}
	},
	error = function(e) { 
		print(e)
		print("Could not upload graph data")
	})
}

tryCatch({
	nd_req <- RETRY("GET", url=Sys.getenv("ND_URL"), times=5, pause_min=2)
	nd <- summarise_nd_from_data_req(nd_req)
	nd_error <- http_error(nd_req)
	},
	error = function(e) { 
		print(e)
		print("Could not fetch terminal web data")
		nd_error <- http_error(nd_req)
	},
	warning = function(w) {
		print('nd fetch warning')
		print(w)
	}
)

if (!nd_error) {
	tryCatch({
		dbWriteTable(con, 'nd', nd$agg, row.names=FALSE, overwrite=FALSE, append=TRUE)
		dbWriteTable(con, 'nd_bal', nd$bal, row.names=FALSE, overwrite=TRUE)
		dbWriteTable(con, 'nd_fail', nd$state, row.names=FALSE, overwrite=TRUE)
		dbExecute(con, 'delete from nd where "time" <= (now() at time zone \'utc\')  - interval \'3 month\'')
		},
		error = function(e) { 
			print(e)
			print("Could not upload terminal web data")
		},
		warning = function(w) {
			print('nd commit warning')
			print(w)
		}
	)
}

tryCatch({
	conn <- GraphqlClient$new(url=Sys.getenv("AMBOSS_URL"))
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
	data_req <- conn$exec(new$link) %>% fromJSON(flatten=F)
	amboss_communities <- summarise_amboss_communities_from_data_req(data_req)
	},
	error = function(e) { 
		print(e)
		print("Could not fetch amboss data")
	},
	warning = function(w) {
		print('amboss communities warning')
		print(w)
	}
)

if (exists("amboss_communities") && nrow(amboss_communities) > 0) {
	tryCatch({
		dbWriteTable(con, 'communities', amboss_communities, row.names=FALSE, overwrite=TRUE)
		},
		error = function(e) { 
			print(e)
			print("Could not upload amboss data")
		},
		warning = function(w) {
			print('amboss db commit warning')
			print(w)
		}
	)
}

tryCatch({
	dbDisconnect(con)
	print("Successfully closed DB connection")
	},
	error = function(e) { 
		print(e)
		stop("Could not close connection")
	},
	warning = function(w) {
		print('warning with db close')
		print(w)
	}
)
