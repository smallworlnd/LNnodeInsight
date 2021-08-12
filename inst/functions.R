recompute_centralities <- function(prev_g, new_g) {
	new_g_filt <- new_g %>%
		filter(inact.channels/num.channels<.5, tot.capacity>heuristics$q1capacity, num.channels>heuristics$q1num.channels) %>%
		select(-c(cent.between, cent.close, cent.eigen))
		ids <- new_g_filt %>%
		as_tibble %>%
		select(id) %>%
		pull
	cent.betw <- centr_betw(new_g_filt, directed=FALSE)
	cent.clo <- centr_clo(new_g_filt, mode='all')
	cent.eigen <- centr_eigen(new_g_filt, directed=FALSE)
	cent_summ <- cbind(ids, cent.betw$res, cent.clo$res, cent.eigen$vector) %>%
		as_tibble %>%
		rename(c('id'='ids', 'cent.between'='V2', 'cent.close'='V3', 'cent.eigen'='V4')) %>%
		mutate(sim.cent.between.rank=rank(-cent.between, ties.method='first'), sim.cent.eigen.rank=rank(-cent.eigen, ties.method='first'), sim.cent.close.rank=rank(-cent.close, ties.method='first'))
	new_g_filt <- left_join(new_g_filt, cent_summ, by='id') %>%
		mutate(cent.between.rank.delta=cent.between.rank-sim.cent.between.rank, cent.close.rank.delta=cent.close.rank-sim.cent.close.rank, cent.eigen.rank.delta=cent.eigen.rank-sim.cent.eigen.rank)
	centralities <- list(new_g_filt, cent.betw, cent.clo, cent.eigen)
	names(centralities) <- c('graph', 'betw', 'clo', 'eigen')
	return(centralities)
}

peer_graph <- function(graph, s_node) {
	return((make_ego_graph(graph, order=1, mode='all', nodes=s_node))[[1]] %>%
		as_tbl_graph)
}

fetch_pubkey <- function(pubkey_or_alias) {
	return(
		ifelse(grepl('^\\w{66}$', pubkey_or_alias), pubkey_or_alias, 
			ifelse(pubkey_or_alias == "", NA, g %>%
				filter(alias==pubkey_or_alias) %>%
				select(name) %>%
				pull %>%
				as.vector
			)
		)
	)
}

sim_chan <- function(s_node, t_node, indel, amount=5e6) {
	t_node_req <- data.frame(t_node, indel)
	s_id <- g %>%
		filter(name %in% s_node) %>%
		select(id) %>%
		pull %>%
		as.vector
	t_node_id <- g %>%
		filter(name %in% t_node) %>%
		select(name, id) %>%
		as_tibble
	t_node_id <- left_join(t_node_id, t_node_req, by=c('name'='t_node'))
	add <- t_node_id %>% filter(indel=='add') %>% select(id) %>% pull
	rem <- t_node_id %>% filter(indel=='remove') %>% select(id) %>% pull
	if (length(add) > 0 && length(rem) == 0) {
		g_mod <- bind_edges(g, data.frame(from=s_id, to=add, capacity=amount))
	}
	else if (length(add) > 0 && length(rem) > 0) {
		g_mod <- bind_edges(g, data.frame(from=s_id, to=add, capacity=amount))
		del_edges <- g %>% activate(edges) %>% filter((from==s_id & to %in% rem) | (from %in% rem & to==s_id)) %>% as_tibble %>% select(to, from, capacity)
		g_mod <- delete_edges(g_mod, E(g)[del_edges$from %--% del_edges$to]) %>%
			as_tbl_graph
	}
	else {
		del_edges <- g %>% activate(edges) %>% filter((from==s_id & to %in% rem) | (from %in% rem & to==s_id)) %>% as_tibble %>% select(to, from, capacity)
		g_mod <- delete_edges(g, E(g)[del_edges$from %--% del_edges$to]) %>%
			as_tbl_graph
	}
	sim_g <- recompute_centralities(g, g_mod)
	return(sim_g)
}
