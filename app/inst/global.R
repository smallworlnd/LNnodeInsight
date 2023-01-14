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

nodes_current <- pool %>% tbl('nodes_current') %>% as_tibble
edges_current <- pool %>% tbl('edges_current') %>% as_tibble

nd_current <- pool %>% tbl('nd') %>% filter(time==max(time)) %>% as_tibble
bos_current <- pool %>% tbl('bos') %>% filter(time==max(time)) %>% as_tibble

# global for nodestatsApp
ln_summary_stats <- tbl(pool, "nodes_historical") %>%
	filter(mean.rate.ppm.out<20e3, mean.rate.ppm.in<20e3) %>%
	group_by(time) %>%
	summarise(
		tot.capacity=mean(tot.capacity, na.rm=TRUE),
		mean.rate.ppm.out=mean(mean.rate.ppm.out, na.rm=TRUE),
		mean.rate.ppm.in=mean(mean.rate.ppm.in, na.rm=TRUE),
		avg.capacity=mean(avg.capacity, na.rm=TRUE)) %>%
	arrange(time) %>%
	as_tibble

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
