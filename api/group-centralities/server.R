library(keyplayer)
comms <- tbl(con, 'communities') %>% as_tibble %>% mutate(community=as.factor(community))
comms_list <- comms %>% dplyr::select(community) %>% distinct(community) %>% pull
comms_list <- comms_list[non_empty]

adj.mat <- as_adjacency_matrix(g_heur)

metrics <- c('betweenness', 'closeness', 'evcent')
cents <- lapply(
	metrics,
	function(x) lapply(
		comms_list,
		function(y)
			kpcent(
				adj.mat,
				which(adj.mat@Dimnames[[1]] %in% (comms %>% filter(community %in% y) %>% pull(pubkey))),
				cmode='all',
				type=x
			)
		) %>%
		unlist
	) %>%
	bind_cols
combined <- cbind(comms_list, cents)
names(combined) <- c('community', 'betweenness', 'closeness', 'evcent')
