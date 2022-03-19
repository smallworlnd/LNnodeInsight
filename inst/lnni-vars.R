customjs <- read_file("js/networkd3.js")

table_vars <- c('Pubkey'='name',
	'Alias'='alias',
	'Total capacity (sat)'='tot.capacity',
	'Number of channels'='num.channels',
	'Average channel capacity (sat)'='avg.capacity',
	'Median channel capacity (sat)'='med.capacity',
	'Mean base fee (msat)'='mean.base.msat',
	'Median base fee (msat)'='median.base.msat',
	'Mean fee rate (ppm)'='mean.rate.ppm',
	'Median fee rate (ppm)'='median.rate.ppm',
	'Approximate node age (days)'='age',
	'Number of active channels'='act.channels',
	'Number of inactive channels'='inact.channels',
	'Betweenness centrality rank'='cent.between.rank',
	'Eigenvector centrality rank'='cent.eigen.rank',
	'Closeness centrality rank'='cent.close.rank',
	'Terminal Web score'='tweb.score',
	'BOS score'='bos')
chart_vars <- c('Total capacity (sat)'='tot.capacity',
	'Number of channels'='num.channels',
	'Average channel capacity (sat)'='avg.capacity',
	'Median channel capacity (sat)'='med.capacity',
	'Mean base fee (msat)'='mean.base.msat',
	'Median base fee (msat)'='median.base.msat',
	'Mean fee rate (ppm)'='mean.rate.ppm',
	'Median fee rate (ppm)'='median.rate.ppm',
	'Number of active channels'='act.channels',
	'Number of inactive channels'='inact.channels',
	'Approximate node age (days)'='age',
	'Betweenness centrality'='cent.between',
	'Eigenvector centrality'='cent.eigen',
	'Closeness centrality'='cent.close',
	'Terminal Web score'='tweb.score',
	'BOS score'='bos')

#nodes_historical <- pool %>% tbl('nodes_historical') %>% as_tibble
nodes_current <- pool %>% tbl('nodes_current') %>% as_tibble
edges_current <- pool %>% tbl('edges_current') %>% as_tibble

nd_current <- pool %>% tbl('nd') %>% filter(time==max(time)) %>% as_tibble
bos_current <- pool %>% tbl('bos') %>% filter(time==max(time)) %>% as_tibble

undir_graph <- build_graph(nodes_current, edges_current)

node_ids <- nodes_current %>%
	mutate(alias_pubkey=paste(alias, "-", pubkey)) %>%
	pull(alias_pubkey)

store_headers <- add_headers(c(
	"Content-Type"=paste("application/json"),
	"Authorization"=paste("token", Sys.getenv("STORE_API_KEY"), sep=" ")))

rest_headers <- add_headers(c("Grpc-Metadata-macaroon"=Sys.getenv("VERIFY_MACAROON")))
