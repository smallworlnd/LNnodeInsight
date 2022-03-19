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
	# fetch screenshot time of the graph
	ss.time <- data_req$date
	# pull graph content from request
	dg_resp <- content(data_req, as="text") %>% fromJSON(flatten=TRUE)

	# fetch nodes
	nodes <- dg_resp$nodes %>%
		unnest(addresses) %>%
		dplyr::select(pub_key, alias) %>%
		rename('pubkey'='pub_key') %>%
		unique
	# fetch links
	edges <- dg_resp$edges %>%
		mutate(
			capacity=as.numeric(capacity),
			node1_policy.fee_base_msat=as.numeric(node1_policy.fee_base_msat),
			node1_policy.fee_rate_milli_msat=as.numeric(node1_policy.fee_rate_milli_msat),
			node2_policy.fee_base_msat=as.numeric(node2_policy.fee_base_msat),
			node2_policy.fee_rate_milli_msat=as.numeric(node2_policy.fee_rate_milli_msat)) %>%
		separate(chan_point, sep=':', into='chan_point', extra='drop')
	# fetch total capacity per node
	n1 <- edges %>%
		dplyr::select(node1_pub, capacity) %>%
		rename(c('pubkey'='node1_pub'))
	n2 <- edges %>%
		dplyr::select(node2_pub, capacity) %>%
		rename(c('pubkey'='node2_pub'))
	capacity <- rbind(n1, n2) %>%
		dplyr::group_by(pubkey) %>%
		summarise(
			tot.capacity=sum(capacity),
			num.channels=n(),
			avg.capacity=mean(capacity),
			med.capacity=median(capacity))
	# fetch number of dead channels, i.e., >14 days since last update
	u1 <- edges %>%
		dplyr::group_by(node1_pub) %>%
		mutate(
			diff=as.numeric(ss.time - as_datetime(last_update), unit='days'),
			state=ifelse(diff>14, "inact.channels", "act.channels")) %>%
		dplyr::select(node1_pub, state) %>%
		rename(c('pubkey'='node1_pub'))
	u2 <- edges %>%
		dplyr::group_by(node2_pub) %>%
		mutate(
			diff=as.numeric(ss.time - as_datetime(last_update), unit='days'),
			state=ifelse(diff>14, "inact.channels", "act.channels")) %>%
		dplyr::select(node2_pub, state) %>%
		rename(c('pubkey'='node2_pub'))
	chanstates <- rbind(u1, u2) %>%
		group_by(pubkey, state) %>%
		summarise(count=n()) %>%
		pivot_wider(names_from=state, values_from=count, values_fill=0)
	# pull out node fees and compute the averages
	f1 <- edges %>%
		group_by(node1_pub) %>%
		mutate(node1.mean.basefee=mean(node1_policy.fee_base_msat, na.rm=TRUE)) %>%
		dplyr::select(node1_pub, node1_policy.fee_base_msat, node1_policy.fee_rate_milli_msat) %>%
		mutate(
			node1_policy.fee_base_msat=ifelse(is.finite(node1_policy.fee_base_msat), node1_policy.fee_base_msat, NA),
			node1_policy.fee_rate_milli_msat=ifelse(is.finite(node1_policy.fee_rate_milli_msat), node1_policy.fee_rate_milli_msat, NA))
	names(f1) <- c('pubkey', 'base.msat', 'rate.ppm')
	f2 <- edges %>%
		group_by(node2_pub) %>%
		mutate(node2.mean.basefee=mean(node2_policy.fee_base_msat, na.rm=TRUE)) %>%
		dplyr::select(node2_pub, node2_policy.fee_base_msat, node2_policy.fee_rate_milli_msat) %>%
		mutate(
			node2_policy.fee_base_msat=ifelse(is.finite(node2_policy.fee_base_msat), node2_policy.fee_base_msat, NA),
			node2_policy.fee_rate_milli_msat=ifelse(is.finite(node2_policy.fee_rate_milli_msat), node2_policy.fee_rate_milli_msat, NA))
	names(f2) <- c('pubkey', 'base.msat', 'rate.ppm')
	fees <- rbind(f1, f2) %>%
		group_by(pubkey) %>%
		summarise(
			mean.base.msat=mean(base.msat, na.rm=TRUE),
			mean.rate.ppm=mean(rate.ppm, na.rm=TRUE),
			median.base.msat=median(base.msat, na.rm=TRUE),
			median.rate.ppm=median(rate.ppm, na.rm=TRUE))
	# join node information
	nodes <- list(nodes, capacity, fees, chanstates) %>% 
		reduce(left_join, by="pubkey")
	# build graph
	channels <- edges %>%
		dplyr::select(node1_pub, node2_pub, capacity, last_update, node1_policy.fee_base_msat, node1_policy.fee_rate_milli_msat, node2_policy.fee_base_msat, node2_policy.fee_rate_milli_msat) %>%
		mutate(last_update=as.numeric(as_datetime(max(last_update, na.rm=TRUE)) - as_datetime(last_update), units='days')) %>%
		rename(c('from_base_fee'='node1_policy.fee_base_msat', 'from_fee_rate'='node1_policy.fee_rate_milli_msat', 'to_base_fee'='node2_policy.fee_base_msat', 'to_fee_rate'='node2_policy.fee_rate_milli_msat', 'from'='node1_pub', 'to'='node2_pub')) %>%
		as_tibble

	# expand single edges into 2 to express asymmetric chann properties
	rev_edges <- channels %>%
		mutate(f=to, t=from, fbf=to_base_fee, tbf=from_base_fee, ff=to_fee_rate, tf=from_fee_rate, direction=0) %>%
		dplyr::select(f, t, capacity, last_update, fbf, ff, tbf, tf, direction) %>%
		rename(c('from'='f', 'to'='t', 'from_base_fee'='fbf', 'from_fee_rate'='ff', 'to_base_fee'='tbf', 'to_fee_rate'='tf'))
	forw_edges <- channels %>% mutate(direction=1)
	all_edges <- rbind(forw_edges, rev_edges)
	g <- as_tbl_graph(all_edges, directed=TRUE, node_key='pubkey') %>%
		rename('pubkey'='name') %>%
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

	nodes <- list(nodes, g_cent_summ, g_cent_w_summ) %>% 
		reduce(left_join, by="pubkey") %>%
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
	ss.time <- data_req$date
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

summarise_bos_from_data_req <- function(data_req) {
	ss.time <- data_req$date
	bos_resp <- content(data_req, as="text") %>% fromJSON(flatten=TRUE)

	bos <- bos_resp$scores %>%
		dplyr::select(public_key, score) %>%
		rename(c('pubkey'='public_key')) %>%
		mutate(time=as_datetime(ss.time)) %>%
		as_tibble
	return(bos)
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
		print(w)
	}
)

graph_headers <- add_headers(c("Grpc-Metadata-macaroon"=Sys.getenv("GRAPH_MACAROON")))
tryCatch({
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

# upload latest graph data to the db
if (!graph_error) {
	tryCatch({
		dbWriteTable(con, 'nodes_historical', latest_graph$nodes, row.names=FALSE, overwrite=FALSE, append=TRUE)
		dbExecute(con, 'delete from nodes_historical where "time" <= now() - interval \'3 month\'')
		dbWriteTable(con, 'nodes_current', latest_graph$nodes, row.names=FALSE, overwrite=TRUE)
		dbWriteTable(con, 'edges_current', latest_graph$channels, row.names=FALSE, overwrite=TRUE)
		},
		error = function(e) { 
			print(e)
			print("Could not upload graph data")
		},
		warning = function(w) {
			print(w)
		}
	)
}

# upload new users to db
if (!graph_error) {
	tryCatch({
		users_in_db <- tbl(con, 'users') %>% as_tibble %>% dplyr::select(pubkey, alias)
		users_in_graph <- latest_node_summary %>% dplyr::select(pubkey, alias)
		new_users <- setdiff(users_in_graph$pubkey, users_in_db$pubkey)
		if (length(new_users) > 0) {
			users_to_add <- users_in_graph %>% filter(pubkey %in% new_users) %>% dplyr::select(pubkey, alias)
			permissions <- 'standard'
			new_users_df <- data.frame(users_to_add, permissions)
			names(new_users_df) <- c('pubkey', 'alias', 'permissions')
			dbWriteTable(con, 'users', new_users_df, row.names=FALSE, overwrite=FALSE, append=TRUE)
		}

		},
		error = function(e) { 
			print(e)
			print("Could not upload new users")
		},
		warning = function(w) {
			print(w)
		}
	)
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
		print(w)
	}
)

if (!nd_error) {
	tryCatch({
		dbWriteTable(con, 'nd', nd$agg, row.names=FALSE, overwrite=FALSE, append=TRUE)
		dbWriteTable(con, 'nd_bal', nd$bal, row.names=FALSE, overwrite=TRUE)
		dbWriteTable(con, 'nd_fail', nd$state, row.names=FALSE, overwrite=TRUE)
		dbExecute(con, 'delete from nd where "time" <= now() - interval \'3 month\'')
		},
		error = function(e) { 
			print(e)
			print("Could not upload terminal web data")
		},
		warning = function(w) {
			print(w)
		}
	)
}

tryCatch({
	bos_req <- RETRY("GET", url=Sys.getenv("BOS_URL"), times=5, pause_min=2)
	bos <- summarise_bos_from_data_req(bos_req)
	bos_error <- http_error(dg_req)
	},
	error = function(e) { 
		print(e)
		print("Could not fetch bos data")
		bos_error <- http_error(dg_req)
	},
	warning = function(w) {
		print(w)
	}
)

if (!bos_error) {
	tryCatch({
		dbWriteTable(con, 'bos', bos, row.names=FALSE, overwrite=FALSE, append=TRUE)
		dbExecute(con, 'delete from bos where "time" <= now() - interval \'3 month\'')
		},
		error = function(e) { 
			print(e)
			print("Could not upload bos data")
		},
		warning = function(w) {
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
		print(w)
	}
)

if (exists("amboss_communities") && nrow(amboss_communities) > 0) {
	tryCatch({
		dbWriteTable(con, 'communities', amboss_communities, row.names=FALSE, overwrite=FALSE, append=TRUE)
		},
		error = function(e) { 
			print(e)
			print("Could not upload amboss data")
		},
		warning = function(w) {
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
		print(w)
	}
)
