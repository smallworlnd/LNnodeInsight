simulate_channel <- function(graph, subject_pubkey, target_pubkeys, indel, amount=5e6) {
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
	}
	else if (length(add) > 0 && length(rem) > 0) {
		graph_mod <- bind_edges(graph, data.frame(from=s_id, to=add, capacity=amount))
		del_edges <- graph_mod %>% activate(edges) %>% filter((from==s_id & to %in% rem) | (from %in% rem & to==s_id)) %>% as_tibble %>% dplyr::select(to, from, capacity)
		graph_mod <- delete_edges(graph_mod, E(graph_mod)[del_edges$from %--% del_edges$to]) %>% as_tbl_graph
	}
	else {
		del_edges <- graph %>% activate(edges) %>% filter((from==s_id & to %in% rem) | (from %in% rem & to==s_id)) %>% as_tibble %>% dplyr::select(to, from, capacity)
		graph_mod <- delete_edges(graph, E(graph)[del_edges$from %--% del_edges$to]) %>% as_tbl_graph
	}

	# recompute centralities on this modified graph
	graph_mod_filt <- graph_mod %>%
		filter(act.channels>0, tot.capacity>70e3, num.channels>1) %>%
		mutate(id=row_number()) %>%
		dplyr::select(-c(cent.between, cent.close, cent.eigen))
	ids <- graph_mod_filt %>%
		as_tibble %>%
		pull(id)
	cent.betw <- centr_betw(graph_mod_filt, directed=FALSE)
	cent.clo <- centr_clo(graph_mod_filt, mode='all')
	cent.eigen <- centr_eigen(graph_mod_filt, directed=FALSE)
	cent_summ <- cbind(ids, cent.betw$res, cent.clo$res, cent.eigen$vector) %>%
		as_tibble %>%
		rename(c('id'='ids', 'cent.between'='V2', 'cent.close'='V3', 'cent.eigen'='V4')) %>%
		mutate(sim.cent.between.rank=rank(-cent.between, ties.method='first'), sim.cent.eigen.rank=rank(-cent.eigen, ties.method='first'), sim.cent.close.rank=rank(-cent.close, ties.method='first'))
	graph_mod_filt <- left_join(graph_mod_filt, cent_summ, by='id') %>%
		mutate(cent.between.rank.delta=cent.between.rank-sim.cent.between.rank, cent.close.rank.delta=cent.close.rank-sim.cent.close.rank, cent.eigen.rank.delta=cent.eigen.rank-sim.cent.eigen.rank)
	sim_res <- list(graph_mod_filt, cent.betw, cent.clo, cent.eigen)
	names(sim_res) <- c('graph', 'betw', 'clo', 'eigen')
	return(sim_res)
}
