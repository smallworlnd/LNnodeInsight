#* returns node betweenness, closeness and eigenvector centralities
#* @get /centralityranks

function() {
	g %>% dplyr::select(graph_timestamp, name, cent.between.rank, cent.between.weight.rank, cent.close.rank, cent.close.weight.rank, cent.eigen.rank, cent.eigen.weight.rank) %>% as_tibble %>% rename(c('pubkey'='name', 'cent_between_rank'='cent.between.rank', 'cent_close_rank'='cent.close.rank', 'cent_eigen_rank'='cent.eigen.rank', 'cent_between_weight_rank'='cent.between.weight.rank', 'cent_close_weight_rank'='cent.close.weight.rank', 'cent_eigen_weight_rank'='cent.eigen.weight.rank'))
}
