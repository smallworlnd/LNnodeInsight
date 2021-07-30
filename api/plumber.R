#* returns node betweenness, closeness and eigenvector centralities
#* @get /centralityranks

function() {
	g %>% select(name, cent.between.rank, cent.close.rank, cent.eigen.rank) %>% as_tibble %>% rename(c('pubkey'='name', 'cent_between_rank'='cent.between.rank', 'cent_close_rank'='cent.close.rank', 'cent_eigen_rank'='cent.eigen.rank'))
}
