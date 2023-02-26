#* Nodes/Channels/Capacity
#* Pull historical summary information about publicly announced total capacity, nodes and channels
#* @tag summaryStats
#* @get /ln-summary-ts
#* @response 200 Historical summary information on total capacity, number of nodes and channels
function() {
	ln <- tbl(pool, "ln_summary_ts") %>%
		as_tibble %>%
		nest_by(time, .key="stats")
	return(list(ln_summary=ln))
}

#* Centralities
#* Pull current centrality information computed by LNnodeInsight for all publicly visible nodes
#* @tag summaryStats
#* @get /old/centralities
#* @serializer json
#* @response 200 Current centrality information for publicly visible nodes
function() {
	tbl(pool, "nodes_current") %>%
		dplyr::select(
			time,
			pubkey,
			cent.between.rank,
			cent.between.weight.rank,
			cent.close.rank,
			cent.close.weight.rank,
			cent.eigen.rank,
			cent.eigen.weight.rank) %>%
		as_tibble %>%
		rename(c(
			'graph_timestamp'='time',
			'cent_between_rank'='cent.between.rank',
			'cent_close_rank'='cent.close.rank',
			'cent_eigen_rank'='cent.eigen.rank',
			'cent_between_weight_rank'='cent.between.weight.rank',
			'cent_close_weight_rank'='cent.close.weight.rank',
			'cent_eigen_weight_rank'='cent.eigen.weight.rank'))
}

#* Centralities
#* Pull current centrality information computed by LNnodeInsight for all publicly visible nodes
#* @tag summaryStats
#* @get /centralities
#* @response 200 Current centrality information for publicly visible nodes
function() {
	cents <- tbl(pool, "nodes_current") %>%
		dplyr::select(
			time,
			pubkey,
			cent.between.rank,
			cent.between.weight.rank,
			cent.close.rank,
			cent.close.weight.rank,
			cent.eigen.rank,
			cent.eigen.weight.rank) %>%
		as_tibble %>%
		mutate(unixtime=as.numeric(as.POSIXct(time))) %>%
		dplyr::select(-time) %>%
		rename(c(
			'graph_timestamp'='unixtime',
			'cent_between_rank'='cent.between.rank',
			'cent_close_rank'='cent.close.rank',
			'cent_eigen_rank'='cent.eigen.rank',
			'cent_between_weight_rank'='cent.between.weight.rank',
			'cent_close_weight_rank'='cent.close.weight.rank',
			'cent_eigen_weight_rank'='cent.eigen.weight.rank')) %>%
		nest_by(pubkey, .key="ranks")
	return(list(centralities=cents))
}
