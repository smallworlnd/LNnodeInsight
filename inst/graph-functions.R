build_graph <- function(nodes, channels, direct=FALSE) {
	if (direct) {
		links <- channels
	} else {
		links <- channels %>% filter(direction==1)
	}
	graph <- as_tbl_graph(links, directed=direct) %>%
		rename('id'='name') %>%
		mutate(id=as.numeric(id)) %>%
		left_join(., nodes, by='id')
	return(graph)
}

peer_graph <- function(graph=undir_graph, s_node) {
	return((make_ego_graph(graph, order=1, mode='all', nodes=s_node))[[1]] %>%
		as_tbl_graph)
}

# fetch neighborhood fees
fetch_peer_fees <- function(graph=undir_graph, pubkey) {
	pubkey_id <- fetch_id(graph, pubkey)
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

fetch_alias <- function(nodes=nodes_current, pubkey) {
	return(nodes %>% filter(pubkey==!!pubkey) %>% pull(alias))
}

fetch_alias_from_id <- function(nodes=nodes_current, node_id) {
	return(nodes %>% filter(id==!!node_id) %>% pull(alias))
}

fetch_peer_aliases <- function(graph=undir_graph, pubkey) {
	peers <- adjacent_vertices(graph, fetch_id(pubkey=pubkey), mode='all') %>%
		unlist %>% as.vector
	peer_aliases <- graph %>% filter(id %in% peers) %>% pull(alias)
	return(peer_aliases)
}

fetch_id <- function(graph=undir_graph, pubkey) {
	return(graph %>% filter(pubkey==!!pubkey) %>% pull(id))
}

fetch_peers_of_peers <- function(graph=undir_graph, pubkey) {
	peer_pubkeys <- adjacent_vertices(graph, pubkey, mode='all') %>%
		unlist %>%
		enframe %>%
		separate(name, c('pubkey', 'peer_pubkey')) %>%
		pull(peer_pubkey)
	peer_pubkey_aliases <- graph %>%
		filter(name %in% peer_pubkeys) %>%
		dplyr::select(name, alias, num.channels) %>%
		as_tibble
	peers_of_peers <- lapply(peer_pubkey_aliases$name, function(x) fetch_peer_aliases(graph, x) %>% unique)
	return(peers_of_peers)
}

fetch_peer_overlaps <- function(graph=undir_graph, pubkey) {
	peers_of_peers <- fetch_peers_of_peers(graph, pubkey)
	num_common_peers <- lapply(peers_of_peers, function(x) length(Reduce(intersect, list(x, peer_pubkey_aliases$alias)))) %>% unlist
	df <- data.frame(peer_pubkey_aliases, num_common_peers) %>% arrange(desc(num_common_peers))
	return(df)
}

path_flow_cost <- function(graph, subject, out_node, in_node, max_samp=1000) {
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

perc_nonNA <- function(vec) {
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
