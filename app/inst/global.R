source("inst/graph-functions.R")

tryCatch({
	pool <- dbPool(
			drv=PostgreSQL(),
			host=Sys.getenv("DB_HOST"),
			port=Sys.getenv("DB_PORT"),
			dbname=Sys.getenv("DB_NAME"),
			user=Sys.getenv("DB_USER"),
			password=Sys.getenv("DB_PW"))
	},
	error = function(e) { 
		print(e)
		stop("Could not make connection to db")
	},
	warning = function(w) {
		print(w)
	}
)

chansim_filters <- data.frame(
	filter_vars=c("max.cap", "max.avg.capacity", "max.fee.rate", "max.num.channels", "max.between", "max.close", "max.eigen", "max.hops", "max.lnplus.rank"),
	filter_max=tbl(pool, 'nodes_current') %>%
		summarise(
			max.cap=round(max(tot.capacity)/1e8+1, 0),
			max.avg.capacity=max(avg.capacity)/1e8,
			max.fee.rate=6000,
			max.num.channels=max(num.channels)+1,
			max.between=max(cent.between.rank),
			max.close=max(cent.close.rank),
			max.eigen=max(cent.eigen.rank),
			max.hops=11,
			max.lnplus.rank=10) %>%
		as_tibble %>%
		unlist(use.names=FALSE),
	filter_descr=c(
		'Filter by range of total capacity (in BTC)',
		'Filter by range of average channel capacity (in BTC)',
		'Filter by range of average channel fee rates (ppm)',
		'Filter by range of total channels',
		'Filter by range of betweenness centrality ranks',
		'Filter by range of closeness centrality ranks',
		'Filter by range of eigenvector centrality ranks',
		'Only show nodes that fall within a range of hops away from the node selected in Step 1',
		"Filter by range of LightningNetwork+ ranks"),
	filter_min=c(0.01, 0.001, 0, 1, 1, 1, 1, 0, 1),
	filter_steps=c(0.1, 0.01, 1, 1, 1, 1, 1, 1, 1)
	) %>% t %>% as.data.frame

report_filters <- data.frame(
	filter_vars=c("max.cap", "max.med.capacity", "max.fee.rate", "max.num.channels", "max.between", "max.close", "max.eigen", "max.hops"),
	filter_max=tbl(pool, 'nodes_current') %>%
		summarise(
			max.cap=round(max(tot.capacity)/1e8+1, 0),
			max.med.capacity=max(med.capacity)/1e8,
			max.fee.rate=6000,
			max.num.channels=max(num.channels)+1,
			max.between=max(cent.between.rank),
			max.close=max(cent.close.rank),
			max.eigen=max(cent.eigen.rank),
			max.hops=11) %>%
		as_tibble %>%
		unlist(use.names=FALSE),
	filter_descr=c(
		'Filter by range of total capacity (in BTC)',
		'Filter by range of median channel capacity (in BTC)',
		'Filter by range of median channel fee rates (ppm)',
		'Filter by range of total channels',
		'Filter by range of betweenness centrality ranks',
		'Filter by range of closeness centrality ranks',
		'Filter by range of eigenvector centrality ranks',
		'Search nodes that fall within a range of hops away from your node'),
	filter_min=c(0.1, 0.005, 0, 5, 1, 1, 1, 1),
	filter_steps=c(0.1, 0.01, 1, 1, 1, 1, 1, 1)
	) %>% t %>% as.data.frame

chansim_api_info <- if (Sys.getenv("LOCAL")) {
		list(url=Sys.getenv("CHANSIM_LOCAL_API_URL"))
	} else {
		get_api_info("chansim-api")
	}
rebalsim_api_info <- if (Sys.getenv("LOCAL")) {
		list(url=Sys.getenv("REBALSIM_LOCAL_API_URL"))
	} else {
		get_api_info("rebalsim-api")
	}
capfeesim_api_info <- if (Sys.getenv("LOCAL")) {
		list(url=Sys.getenv("CAPFEESIM_LOCAL_API_URL"))
	} else {
		get_api_info("capfeesim-api")
	}
lnplus_swap_minmax_api_info <- if (Sys.getenv("LOCAL")) {
		list(url=Sys.getenv("LNPLUS_MINMAX_LOCAL_API_URL"))
	} else {
		get_api_info("lnplus-swap-minmax")
	}


nodes_current <- pool %>% tbl('nodes_current') %>% as_tibble
edges_current <- pool %>% tbl('edges_current') %>% as_tibble

nd_current <- pool %>% tbl('nd') %>% filter(time==max(time)) %>% as_tibble
bos_current <- pool %>% tbl('bos') %>% filter(time==max(time)) %>% as_tibble

undir_graph <- build_graph(nodes_current, edges_current)

node_ids <- nodes_current %>%
	filter(act.channels>0 & act.channels/(act.channels+inact.channels)>0.66) %>%
	mutate(alias_pubkey=paste(alias, "-", pubkey)) %>%
	pull(alias_pubkey)

comms_list <- tbl(pool, 'communities') %>% distinct(community) %>% arrange %>% pull

store_headers <- add_headers(c(
	"Content-Type"=paste("application/json"),
	"Authorization"=paste("token", Sys.getenv("STORE_API_KEY"), sep=" ")))

rest_headers <- add_headers(c("Grpc-Metadata-macaroon"=Sys.getenv("VERIFY_MACAROON")))
