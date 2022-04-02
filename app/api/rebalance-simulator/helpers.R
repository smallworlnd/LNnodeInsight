fetch_pubkey <- function(alias_pubkey) {
	return(tail(str_split(alias_pubkey, " - ")[[1]], 1))
}

fetch_id_from_pubkey <- function(graph, key) {
	return(V(graph)[V(graph)$pubkey==key] %>% as.numeric)
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

build_path_flow_cost_dist <- function(graph, out_id, in_id, return_fee, max_samp=1000, max_fails=5) {
	fs <- data.frame()
	paths_sampled <- c()
	failed_sample <- 0
	while (TRUE) {
		w <- graph %>%
			activate(edges) %>%
			pull(from_fee_rate)
		sps <- all_shortest_paths(graph, from=out_id, to=in_id, mode='out')$res
		if (length(sps) >= 1000) {
			sps <- sample(sps, 1000)
		}
		untested_paths <- sps[!sps %in% paths_sampled]
		if (length(untested_paths) == 0 && nrow(fs) < max_samp && failed_sample <= max_fails) {
			failed_sample <- failed_sample + 1
			next
		} else if (nrow(fs) >= max_samp || (length(untested_paths) == 0 && failed_sample >= max_fails)) {
			break
		}
		# compute the mean fee of all shortest paths
		path_fee <- lapply(untested_paths, function(x) return_fee + E(graph, path=x)$from_fee_rate %>% sum) %>% unlist
		max_path_flow <- lapply(untested_paths, function(x) E(graph, path=x)$capacity %>% min) %>% unlist
		path_hops <- lapply(untested_paths, function(x) ifelse((length(x) - 2) == 0, 1, length(x) - 2)) %>% unlist
		known_1Mmin <- lapply(untested_paths, function(x) perc_nonNA(E(graph, path=x)$balance)*100) %>% unlist
		fs <- rbind(fs, data.frame(path_fee, max_path_flow, path_hops, known_1Mmin))
		# delete a random internal node edge/channel
		mid_nodes <- sapply(untested_paths, function(x) fetch_rand_mid_edge(x) %>% as_ids) %>% t %>% as.data.frame %>% rename(c('from'='V1', 'to'='V2'))
		del_edges <- data.frame(from=mid_nodes$from, to=mid_nodes$to)
		graph <- delete_edges(graph, E(graph)[del_edges$from %--% del_edges$to]) %>% as_tbl_graph
		paths_sampled <- c(paths_sampled, untested_paths)
	}
	return(fs)
}
