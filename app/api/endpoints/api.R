if(Sys.getenv("PORT") == "") Sys.setenv(PORT = 8080)

#* returns both unweighted and weighted node betweenness, closeness and eigenvector centralities
#* @param db db connection to fetch node information
#* @get /
function(db=pool) {
	db %>%
		tbl("nodes_current") %>%
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

#* returns both unweighted and weighted node betweenness, closeness and eigenvector centralities
#* @param db db connection to fetch node information
#* @get /centralities
#* @serializer json
function(db=pool) {
	db %>%
		tbl("nodes_current") %>%
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

#* returns ln summary stats in timeseries
#* @param db db connection to fetch node information
#* @get /ln-summary-ts
#* @serializer json
function(db=pool) {
	db %>%
		tbl("ln_summary_ts") %>%
		as_tibble
}
