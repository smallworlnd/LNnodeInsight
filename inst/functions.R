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
		ifelse(
			grepl('^\\w{66}$', pubkey_or_alias), pubkey_or_alias, g %>%
				filter(alias==pubkey_or_alias) %>%
				select(name) %>%
				pull %>%
				as.vector))
}

sim_chan <- function(s_node, t_node, channel='add', amount=5e6) {
	# find the rarget/s ids
	s_id <- g %>%
		filter(name==s_node) %>%
		select(id) %>%
		pull %>%
		as.vector
	# conditionally add/remove channels
	t_id <- g %>%
		filter(name==t_node) %>%
		select(id) %>%
		pull %>%
		as.vector
	if (channel=='add') {
		g_mod <- bind_edges(g, data.frame(from=s_id, to=t_id, capacity=amount))
	} else {
		del_edges <- g %>%
			activate(edges) %>%
			filter((from==s_id & to==t_id) | (from==t_id) & to==s_id) %>%
			as_tibble %>%
			select(from, to, capacity)
		g_mod <- delete_edges(g, E(g)[del_edges$from %--% del_edges$to]) %>%
			as_tbl_graph
	}
	sim_g <- recompute_centralities(g, g_mod)
	# make the reduced graph
	return(sim_g)
}
