#* returns node betweenness, closeness and eigenvector centralities
#* @get /centralityranks

function() {
	g %>% select(name, cent.between.rank, cent.close.rank, cent.eigen.rank) %>% as_tibble %>% rename('pubkey'='name')
}
