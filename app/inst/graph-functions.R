#' build a graph object from node data.frame and edges data.frame
#'
#' input edges should be the full complement of forwards/reverse links such that
#' either a directed or undirected graph can be built
#'
#' @param nodes nodes data.frame with 'pubkey' as the node_key field
#' @param channels links between nodes
#' @param direct boolean on directedness of the graph object
#' @return returns tbl_graph object built from links, then left_joined with
#' nodes
#' @export
build_graph <- function(nodes, channels, direct=FALSE) {
	if (direct) {
		links <- channels
	} else {
		links <- channels %>% filter(direction==1)
	}
	graph <- as_tbl_graph(links, directed=direct, node_key='pubkey') %>%
		rename('pubkey'='name') %>%
		left_join(., nodes, by='pubkey') %>%
		mutate(id=row_number())
	return(graph)
}

#' build local graph
#'
#' creates a subgraph centered on a node and its peers, and any links between
#' peers
#'
#' @param graph undirected graph object (igraph/tbl_graph)
#' @param pubkey pubkey of the node on which a local graph will be made
#' @return returns subgraph of pubkey's neighborhood
#' @export
peer_graph <- function(graph=undir_graph, pubkey) {
	local_graph <- make_ego_graph(graph, order=1, mode='all', nodes=fetch_id(pubkey=pubkey))[[1]] %>%
		as_tbl_graph
	return(local_graph)
}

#' fetch peer fees
#' 
#' @param channels links between nodes
#' @param pubkey pubkey of the node on which a local graph will be made
#' @return returns data.frame of pubkey fee, peer fee, peer pubkey and peer
#' alias
#' @export
fetch_peer_fees <- function(channels=edges_current, pubkey) {
	fees <- channels %>%
		filter(from==pubkey) %>%
		mutate(
			subject_fee=from_fee_rate,
			peer_fee=to_fee_rate,
			peer_pubkey=to) %>%
		dplyr::select(subject_fee, peer_fee, peer_pubkey)
	fees$peer_alias <- sapply(fees$peer_pubkey, function(x) fetch_alias(pubkey=x))
	return(fees)
}

#' parse pubkey
#' 
#' @param alias_pubkey string of the format "alias - pubkey"
#' @return returns parsed pubkey as string
#' @export
fetch_pubkey <- function(alias_pubkey) {
	return(tail(str_split(alias_pubkey, " - ")[[1]], 1))
}

#' fetch pubkey
#' 
#' @param pubkey pubkey for which to fetch alias
#' @return returns alias attached to pubkey
#' @export
fetch_alias <- function(nodes=nodes_current, pubkey) {
	return(nodes %>% filter(pubkey==!!pubkey) %>% pull(alias))
}

#' fetch peer aliases
#'
#' @param graph graph object (igraph/tbl_graph) from which to pull data
#' @param pubkey pubkey for which to fetch peer aliases
#' @return returns vector of aliases
#' @export
fetch_peer_aliases <- function(graph=undir_graph, pubkey) {
	peers <- adjacent_vertices(graph, fetch_id(pubkey=pubkey), mode='all') %>%
		unlist %>% as.vector
	peer_aliases <- graph %>% filter(id %in% peers) %>% pull(alias)
	return(peer_aliases)
}

#' fetch pubkey id
#' 
#' @param graph graph object to pull information from
#' @param pubkey pubkey for which to fetch corresponding ID
#' @return returns numeric ID associated with the pubkey
#' @export
fetch_id <- function(graph=undir_graph, pubkey) {
	return(graph %>% filter(pubkey==!!pubkey) %>% pull(id))
}

#' fetch peers of peers
#'
#' @param graph graph object to pull information from
#' @param pubkey pubkey for which to pull peers and peers of peer aliases for
#' @return returns list of peers for each peer of the pubkey
#' @export
fetch_peers_of_peers <- function(graph=undir_graph, pubkey) {
	peer_ids <- adjacent_vertices(graph, fetch_id(pubkey=pubkey), mode='all') %>% unlist
	peer_pubkey_aliases <- graph %>%
		filter(id %in% peer_ids) %>%
		dplyr::select(pubkey, alias, num.channels) %>%
		as_tibble
	peers_of_peers <- lapply(peer_pubkey_aliases$pubkey, function(x) fetch_peer_aliases(graph, x) %>% unique)
	return(peers_of_peers)
}

#' fetch shared peers
#'
#' compute overlaps in peers of peers for a given pubkey; an extension to
#' \link{fetch_peers_of_peers}
#'
#' @param graph graph object to pull information from
#' @param pubkey pubkey for which to compute peers of peers overlaps
#' @return returns a data.frame of pubkey peers in one column and number of
#' common peers in another column
#' @export
fetch_peer_overlaps <- function(graph=undir_graph, pubkey) {
	peers <- fetch_peer_aliases(pubkey=pubkey)
	peers_of_peers <- fetch_peers_of_peers(graph, pubkey)
	num_common_peers <- lapply(peers_of_peers, function(x) length(Reduce(intersect, list(x, peers)))) %>% unlist
	df <- data.frame(peers, num_common_peers) %>% arrange(desc(num_common_peers))
	return(df)
}

#' compute path maximum flows and costs
#'
#' sample a sub-population of paths between any two nodes and compute statistics
#' on the costs of those paths, the maximum possible liquidity that can flow
#' through a path, and the path balancedness
#' @param graph directed graph object to pull information from
#' @param subject optional subject pubkey for fee modelling purposes
#' @param out_node starting node pubkey for path sampling
#' @param in_node ending node pubkey for path sampling
#' @param max_samp maximum expected number of samples (can be higher based on
#' all_shortest_paths output)
#' @return returns data.frame of total path cost (ppm), total path maximum flow,
#' total number of hops needed to reach in_node from out_node, and proportion of
#' path expected to be balanced
#' @export
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

#' percent of vector that is not NA
#'
#' helper function to \link{path_flow_cost}
#'
#' @param vec vector of numerics
#' @return returns fraction of non-NA elements in a vector
#' @export
perc_nonNA <- function(vec) {
	sum(!is.na(vec))/length(vec)
}

#' sample a channel in a given path
#'
#' helper function to \link{path_flow_cost}
#'
#' @param short_path a path object from node A ending at node B
#' @return returns a random internal edge in the path
#' @export
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

#' qr code generator
#'
#' builds a qr matrix to display as a ggplot object
#'
#' @param text input text to transform into qr
#' @param color qr code color; default 'black'
#' @param alpha opacity of the qr; default 1
#' @return returns ggplot object of the qr matrix
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

#' shift indices of vector elements
#'
#' @param x vector
#' @param n n number of positions to shift by (negative or positive)
#' @return returns vector of elements shifted by n positions
#' @export
circular_shift <- function(x, n) {
	# courtesy of George Pipis
	if(n == 0 | n%%length(x) == 0) {
		return(x)
	}
	else if (abs(n) > length(x)) {
		new_n <- (abs(n)%%length(x))*sign(n)
		return(c(tail(x,new_n), head(x,-new_n)))
	}
	else {
		return(c(tail(x,n), head(x,-n)))
	}
}

#' shift vector element to first index
#'
#' @param vec vector
#' @param ele element of a vector to shift to index 1
#' @return returns vector of elements with a specified element as first
#' @export
shift_to_first <- function(vec, ele) {
	pos <- which(vec==ele)
	if (pos == 3) {
		return(circular_shift(vec, 1))
	}
	else if (pos == 2) {
		return(circular_shift(vec, 2))
	}
	else {
		return(vec)
	}
}

#' fetch peer triangles of pubkey
#'
#' @param graph undirected graph object (igraph/tbl_graph)
#' @param pubkey pubkey for which to compute peers peer triangles
#' @return returns data.frame of pubkey triangles with the specified pubkey
#' positioned in first
#' @export
fetch_node_triangles <- function(graph=undir_graph, pubkey) {
	local_graph <- peer_graph(pubkey=pubkey) %>%
		mutate(id=row_number())
	node_id <- fetch_id(graph=local_graph, pubkey=pubkey)
	tri <- triangles(local_graph) %>% as.vector
	ordered_tri <- split(tri, ceiling(seq_along(tri)/3)) %>%
		keep(function(x) node_id %in% x) %>%
		lapply(function(x) shift_to_first(x, node_id)) %>%
		lapply(function(x) V(local_graph)[x]$alias) %>%
		unlist %>%
		matrix(ncol=3, byrow=TRUE) %>%
		as.data.frame %>%
		rename(c('Node'='V1', 'Peer 1'='V2', 'Peer 2'='V3'))
	return(ordered_tri)
}
