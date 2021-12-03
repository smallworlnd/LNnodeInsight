recompute_centralities <- function(graph) {
	graph_filt <- graph %>%
		filter(act.channels>0, tot.capacity>heuristics$q1capacity, num.channels>heuristics$q1num.channels) %>%
		mutate(id=row_number()) %>%
		dplyr::select(-c(cent.between, cent.close, cent.eigen))
	ids <- graph_filt %>%
		as_tibble %>%
		pull(id)
	cent.betw <- centr_betw(graph_filt, directed=FALSE)
	cent.clo <- centr_clo(graph_filt, mode='all')
	cent.eigen <- centr_eigen(graph_filt, directed=FALSE)
	cent_summ <- cbind(ids, cent.betw$res, cent.clo$res, cent.eigen$vector) %>%
		as_tibble %>%
		rename(c('id'='ids', 'cent.between'='V2', 'cent.close'='V3', 'cent.eigen'='V4')) %>%
		mutate(sim.cent.between.rank=rank(-cent.between, ties.method='first'), sim.cent.eigen.rank=rank(-cent.eigen, ties.method='first'), sim.cent.close.rank=rank(-cent.close, ties.method='first'))
	graph_filt <- left_join(graph_filt, cent_summ, by='id') %>%
		mutate(cent.between.rank.delta=cent.between.rank-sim.cent.between.rank, cent.close.rank.delta=cent.close.rank-sim.cent.close.rank, cent.eigen.rank.delta=cent.eigen.rank-sim.cent.eigen.rank)
	centralities <- list(graph_filt, cent.betw, cent.clo, cent.eigen)
	names(centralities) <- c('graph', 'betw', 'clo', 'eigen')
	return(centralities)
}

peer_graph <- function(graph, s_node) {
	return((make_ego_graph(graph, order=1, mode='all', nodes=s_node))[[1]] %>%
		as_tbl_graph)
}

# fetch neighborhood fees
fetch_peer_fees <- function(graph, pubkey) {
	pubkey_id <- fetch_id(g, pubkey)
	fees <- graph %>%
		activate(edges) %>%
		filter(from==pubkey_id | to==pubkey_id) %>%
		mutate(
			subject_fee=ifelse(from==pubkey_id, from_fee_rate, to_fee_rate),
			peer_fee=ifelse(from==pubkey_id, to_fee_rate, from_fee_rate),
			peer_id=ifelse(from==pubkey_id, to, from)) %>%
		as_tibble %>%
		dplyr::select(subject_fee, peer_fee, peer_id)
	fees$peer_alias <- sapply(fees$peer_id, fetch_alias_from_id)
	return(fees)
}

p_abline <- function(x, a=1, b=0) {
	y <- a * x + b
	return(y)
}

fetch_pubkey <- function(alias_pubkey) {
	return(tail(str_split(alias_pubkey, " - ")[[1]], 1))
}

fetch_alias <- function(pubkey) {
	return(g %>% filter(name==pubkey) %>% pull(alias))
}

fetch_alias_from_id <- function(node_id) {
	return(g %>% filter(id==node_id) %>% pull(alias))
}

fetch_peer_aliases <- function(pubkey) {
	peers <- adjacent_vertices(g, pubkey, mode='all') %>% unlist %>% as.vector
	peer_aliases <- g %>% filter(id %in% peers) %>% dplyr::select(alias) %>% as_tibble %>% pull(alias)
	return(peer_aliases)
}

fetch_peers_of_peers <- function(pubkey) {
	peer_pubkeys <- adjacent_vertices(g, pubkey, mode='all') %>%
		unlist %>%
		enframe %>%
		separate(name, c('pubkey', 'peer_pubkey')) %>%
		pull(peer_pubkey)
	peer_pubkey_aliases <- g %>% filter(name %in% peer_pubkeys) %>% dplyr::select(name, alias, num.channels) %>% as_tibble
	peers_of_peers <- lapply(peer_pubkey_aliases$name, function(x) fetch_peer_aliases(x) %>% unique)
}

fetch_peer_overlaps <- function(pubkey) {
	peer_pubkeys <- adjacent_vertices(g, pubkey, mode='all') %>%
		unlist %>%
		enframe %>%
		separate(name, c('pubkey', 'peer_pubkey')) %>%
		pull(peer_pubkey)
	peer_pubkey_aliases <- g %>% filter(name %in% peer_pubkeys) %>% dplyr::select(name, alias, num.channels) %>% as_tibble
	peers_of_peers <- lapply(peer_pubkey_aliases$name, function(x) fetch_peer_aliases(x) %>% unique)
	num_common_peers <- lapply(peers_of_peers, function(x) length(Reduce(intersect, list(x, peer_pubkey_aliases$alias)))) %>% unlist
	df <- data.frame(peer_pubkey_aliases, num_common_peers) %>% arrange(desc(num_common_peers))
	return(df)
}

sim_chan <- function(s_node, t_node, indel, amount=5e6) {
	t_node_req <- data.frame(t_node, indel)
	s_id <- g %>%
		filter(name %in% s_node) %>%
		pull(id)
	t_node_id <- g %>%
		filter(name %in% t_node) %>%
		dplyr::select(name, id) %>%
		as_tibble
	t_node_id <- left_join(t_node_id, t_node_req, by=c('name'='t_node'))
	add <- t_node_id %>% filter(indel=='add') %>% pull(id)
	rem <- t_node_id %>% filter(indel=='del') %>% pull(id)
	if (length(add) > 0 && length(rem) == 0) {
		g_mod <- bind_edges(g, data.frame(from=s_id, to=add, capacity=amount))
	}
	else if (length(add) > 0 && length(rem) > 0) {
		g_mod <- bind_edges(g, data.frame(from=s_id, to=add, capacity=amount))
		del_edges <- g_mod %>% activate(edges) %>% filter((from==s_id & to %in% rem) | (from %in% rem & to==s_id)) %>% as_tibble %>% dplyr::select(to, from, capacity)
		g_mod <- delete_edges(g_mod, E(g_mod)[del_edges$from %--% del_edges$to]) %>% as_tbl_graph
	}
	else {
		del_edges <- g %>% activate(edges) %>% filter((from==s_id & to %in% rem) | (from %in% rem & to==s_id)) %>% as_tibble %>% dplyr::select(to, from, capacity)
		g_mod <- delete_edges(g, E(g)[del_edges$from %--% del_edges$to]) %>% as_tbl_graph
	}
	sim_g <- recompute_centralities(g_mod)
	return(sim_g)
}

path_flow_cost <- function(in_graph, subject, out_node, in_node, max_samp=500) {
	graph <- in_graph
	fs <- data.frame()
	in_node_id <- graph %>% filter(name==fetch_pubkey(in_node)) %>% pull(id)
	out_node_id <- graph %>% filter(name==fetch_pubkey(out_node)) %>% pull(id)
	in_node <- fetch_pubkey(in_node)
	out_node <- fetch_pubkey(out_node)
	# figure out if in_node has channel to subject
	if (subject != "" && are_adjacent(graph, fetch_pubkey(subject), in_node)) {
		subject_id <- graph %>% filter(name==fetch_pubkey(subject)) %>% pull(id)
		return_fee <- E(graph, path=data.frame(from=in_node_id, to=subject_id))$from_fee_rate
	} else {
		# otherwise use median in_node fee
		return_fee <- graph %>% filter(name==in_node) %>% pull(median.rate.ppm)
	}
	# find a single shortest path
	while (TRUE) {
		w <- graph %>% activate(edges) %>% pull(from_fee_rate)
		sps <- all_shortest_paths(graph, from=out_node, to=in_node, mode='out', weights=w)$res
		if (length(sps) == 0 || nrow(fs) >= max_samp) { break }
		# compute the mean fee of all shortest paths
		path_fee <- lapply(sps, function(x) return_fee + E(graph, path=x)$from_fee_rate %>% sum) %>% unlist
		max_path_flow <- lapply(sps, function(x) E(graph, path=x)$capacity %>% min) %>% unlist
		path_hops <- lapply(sps, function(x) ifelse((length(x) - 2) == 0, 1, length(x) - 2)) %>% unlist
		known_1Mmin <- lapply(sps, function(x) perc_nonNA(E(graph, path=x)$balance)*100) %>% unlist
		fs <- rbind(fs, data.frame(path_fee, max_path_flow, path_hops, known_1Mmin))
		# delete a random internal node edge/channel
		mid_nodes <- sapply(sps, function(x) fetch_rand_mid_edge(x) %>% as_ids) %>% t %>% as.data.frame %>% rename(c('from'='V1', 'to'='V2'))
		del_edges <- data.frame(from=sapply(mid_nodes$from, function(x) fetch_id(graph, x)) %>% as.vector, to=sapply(mid_nodes$to, function(x) fetch_id(graph, x)) %>% as.vector)
		graph <- delete_edges(graph, E(graph)[del_edges$from %--% del_edges$to]) %>% as_tbl_graph
	}
	return(fs)
}

perc_nonNA <- function(vec){
	sum(!is.na(vec))/length(vec)
}

fetch_rand_mid_edge <- function(short_path) {
	mid_nodes <- short_path[-c(1, length(short_path))]
	if (length(mid_nodes) == 0) {
		chan <- c(short_path[1], short_path[2])
	} else {
		samp <- mid_nodes[sample(1:length(mid_nodes), 1)]
		rand <- sample(c(-1, 1), 1)
		if (rand == 1) {
			chan <- c(samp, short_path[match(samp, short_path)+1])
		} else {
			chan <- c(short_path[match(samp, short_path)-1], samp)
		}
	}
	return(chan)
}

fetch_id <- function(in_graph, pubkey) {
	return(in_graph %>% filter(name==pubkey) %>% pull(id))
}

ggqrcode <- function(text, color="black", alpha=1) {
	x <- qrencode(text) %>% as.data.frame
	y <- x
	y$id <- rownames(y)
	y <- gather(y, "key", "value", colnames(y)[-ncol(y)])
	y$key = factor(y$key, levels=rev(colnames(x)))
	y$id = factor(y$id, levels=rev(rownames(x)))
	ggplot(y, aes_(x=~id, y=~key)) +
		geom_tile(aes_(fill=~value), alpha=alpha) +
		scale_fill_gradient(low="white", high=color) +
		theme_void() +
		theme(legend.position='none')
}
