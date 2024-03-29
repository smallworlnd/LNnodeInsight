if(Sys.getenv("PORT") == "") Sys.setenv("PORT"=8080)

#* Returns a list of the simulated graph, and new centrality metrics
#* @param db postgres database connection housing the node and edge information
#* @param subject_pubkey pubkey of the subject node we're simulating channels
#* @param target_pubkeys vector of pubkey(s) of target node(s) we want to simulate with
#* @param indel vector add/delete channel instructions
#* @param amount size of channel to be simulated if adding new
#* @post /
#' @serializer tsv
simulate_channel <- function(subject_pubkey, target_pubkeys, indel, amount=5e6) {
	if(any(is.null(subject_pubkey), is.null(target_pubkeys))){
		stop("Must supply subject and target pubkeys")
	}
	nodes <- pool %>% tbl("nodes_current") %>% as_tibble
	links <- pool %>% tbl("edges_current") %>% filter(direction==1) %>% as_tibble
	graph <- as_tbl_graph(links, directed=FALSE, node_key='pubkey') %>%
		rename('pubkey'='name') %>%
		left_join(., nodes, by='pubkey') %>%
		mutate(id=row_number())
	# gather simulation parameters to modify the graph
	target_pubkeys_req <- data.frame(target_pubkeys, indel)
	s_id <- graph %>%
		filter(pubkey %in% subject_pubkey) %>%
		pull(id)
	target_pubkeys_id <- graph %>%
		filter(pubkey %in% target_pubkeys) %>%
		dplyr::select(pubkey, id) %>%
		as_tibble
	target_pubkeys_id <- left_join(target_pubkeys_id, target_pubkeys_req, by=c('pubkey'='target_pubkeys'))
	add <- target_pubkeys_id %>% filter(indel=='add') %>% pull(id)
	rem <- target_pubkeys_id %>% filter(indel=='del') %>% pull(id)
	if (length(add) > 0 && length(rem) == 0) {
		graph_mod <- bind_edges(graph, data.frame(from=s_id, to=add, capacity=amount))
	} else if (length(add) > 0 && length(rem) > 0) {
		graph_mod <- bind_edges(graph, data.frame(from=s_id, to=add, capacity=amount))
		del_edges <- graph_mod %>%
			activate(edges) %>%
			filter((from==s_id & to %in% rem) | (from %in% rem & to==s_id)) %>%
			as_tibble %>%
			dplyr::select(to, from, capacity)
		graph_mod <- delete_edges(graph_mod, E(graph_mod)[del_edges$from %--% del_edges$to]) %>% as_tbl_graph
	} else {
		del_edges <- graph %>%
			activate(edges) %>%
			filter((from==s_id & to %in% rem) | (from %in% rem & to==s_id)) %>%
			as_tibble %>%
			dplyr::select(to, from, capacity)
		graph_mod <- delete_edges(graph, E(graph)[del_edges$from %--% del_edges$to]) %>% as_tbl_graph
	}

	# recompute centralities on this modified graph
	heuristics <- graph %>%
		as_tibble %>%
		dplyr::select(tot.capacity, num.channels) %>%
		summarise(q1capacity=quantile(tot.capacity, 0.25, na.rm=TRUE), q1num.channels=quantile(num.channels, 0.25, na.rm=TRUE))
	graph_mod_filt <- graph_mod %>%
		filter(act.channels>0, act.channels/(act.channels+inact.channels)>0.66, tot.capacity>heuristics$q1capacity, num.channels>heuristics$q1num.channels) %>%
		mutate(id=row_number()) %>%
		dplyr::select(-c(cent.between, cent.close, cent.eigen)) %>%
		mutate(
			cent.between=centrality_betweenness(directed=FALSE),
			cent.close=centrality_closeness(mode='all'),
			cent.eigen=centrality_eigen(directed=FALSE)
		) %>%
		mutate(
			sim.cent.between.rank=rank(-cent.between, ties.method='first'),
			sim.cent.eigen.rank=rank(-cent.eigen, ties.method='first'),
			sim.cent.close.rank=rank(-cent.close, ties.method='first')
		) %>%
		mutate(
			sim.cent.between.rank=ifelse(length(rem)==0 & sim.cent.between.rank>cent.between.rank, cent.between.rank, sim.cent.between.rank),
			sim.cent.eigen.rank=ifelse(length(rem)==0 & sim.cent.eigen.rank>cent.eigen.rank, cent.eigen.rank, sim.cent.eigen.rank),
			sim.cent.close.rank=ifelse(length(rem)==0 & sim.cent.close.rank>cent.close.rank, cent.close.rank, sim.cent.close.rank),
		) %>%
		mutate(
			cent.between.rank.delta=cent.between.rank-sim.cent.between.rank,
			cent.close.rank.delta=cent.close.rank-sim.cent.close.rank,
			cent.eigen.rank.delta=cent.eigen.rank-sim.cent.eigen.rank
		) %>%
		as_tibble
	graph_mod_filt
}
