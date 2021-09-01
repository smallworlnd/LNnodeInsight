recompute_centralities <- function(prev_g, new_g) {
	new_g_filt <- new_g %>%
		filter(inact.channels/num.channels<.5, tot.capacity>heuristics$q1capacity, num.channels>heuristics$q1num.channels) %>%
		mutate(id=row_number()) %>%
		select(-c(cent.between, cent.close, cent.eigen))
	ids <- new_g_filt %>%
		as_tibble %>%
		pull(id)
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
				pull(name) %>%
				as.vector
			)
		)
	)
}

sim_chan <- function(s_node, t_node, indel, amount=5e6) {
	t_node_req <- data.frame(t_node, indel)
	s_id <- g %>%
		filter(name %in% s_node) %>%
		pull(id)
	t_node_id <- g %>%
		filter(name %in% t_node) %>%
		select(name, id) %>%
		as_tibble
	t_node_id <- left_join(t_node_id, t_node_req, by=c('name'='t_node'))
	add <- t_node_id %>% filter(indel=='add') %>% pull(id)
	rem <- t_node_id %>% filter(indel=='del') %>% pull(id)
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

path_cost <- function(in_graph, subject=NULL, out_node, in_node, max_samp=500) {
	graph <- in_graph
	fs <- c()
	in_node_id <- graph %>% filter(name==fetch_pubkey(in_node)) %>% pull(id)
	out_node_id <- graph %>% filter(name==fetch_pubkey(out_node)) %>% pull(id)
	in_node <- fetch_pubkey(in_node)
	out_node <- fetch_pubkey(out_node)
	# figure out if in_node has channel to subject
	if (!is.null(subject) && are_adjacent(graph, fetch_pubkey(subject), in_node)) {
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
		if (length(sps) == 0 || length(fs) >= max_samp) { break }
		# compute the mean fee of all shortest paths
		path_fee <- lapply(sps, function(x) return_fee + E(graph, path=x)$from_fee_rate %>% sum) %>% unlist
		fs <- c(fs, path_fee)
		# delete a random internal node edge/channel
		mid_nodes <- sapply(sps, function(x) fetch_rand_mid_edge(x) %>% as_ids) %>% t %>% as.data.frame %>% rename(c('from'='V1', 'to'='V2'))
		del_edges <- data.frame(from=sapply(mid_nodes$from, function(x) fetch_id(graph, x)) %>% as.vector, to=sapply(mid_nodes$to, function(x) fetch_id(graph, x)) %>% as.vector)
		graph <- delete_edges(graph, E(graph)[del_edges$from %--% del_edges$to]) %>% as_tbl_graph
	}
	return(fs)
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
	x <- qrcode_gen(text, plotQRcode=FALSE, dataOutput=TRUE, softLimitFlag=FALSE)
	x <- as.data.frame(x)
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
