library(tidyverse)
library(tidygraph)
library(jsonlite)
library(igraph)
library(sna)
library(lubridate)
library(intergraph)

# move to the shiny directory
setwd('/srv/shiny-server')

# load in data
graph_fn <- list.files(path="data", pattern="^lnd-graph-[0-9]+.gz")
graph_fn <- paste0('data/', graph_fn[length(graph_fn)])
graph <- fromJSON(graph_fn, flatten=TRUE)
ss.time <- as_datetime((str_split(graph_fn, '-') %>% unlist %>% str_split(".gz") %>% unlist)[3] %>% as.numeric)

nd_fn <- list.files(path="data", pattern="^nd-[0-9]+.json")
nd_fn <- paste0('data/', nd_fn[length(nd_fn)])
nd <- fromJSON(nd_fn)
nd.scored <- nd$scored %>%
	unlist %>%
	as.data.frame %>%
	rownames_to_column %>%
	separate(rowname, into=c('name', 'key'), extra='merge') %>%
	mutate(key=str_replace(key, "peers?[0-9]*", "peer")) %>%
	mutate(key=str_replace(key, "addresses?[0-9]*", "address")) %>%
	as_tibble %>%
	rename('value'='.')
nd.stable <- nd$stable %>%
	unlist %>%
	as.data.frame %>%
	rownames_to_column %>%
	separate(rowname, into=c('name', 'key'), extra='merge') %>%
	mutate(key=str_replace(key, "peers?[0-9]*", "peer")) %>%
	mutate(key=str_replace(key, "addresses?[0-9]*", "address")) %>%
	as_tibble %>%
	rename('value'='.')
nd.unstable <- nd$unstable %>%
	unlist %>%
	as.data.frame %>%
	rownames_to_column %>%
	separate(rowname, into=c('name', 'key'), extra='merge') %>%
	mutate(key=str_replace(key, "peers?[0-9]*", "peer")) %>%
	mutate(key=str_replace(key, "addresses?[0-9]*", "address")) %>%
	as_tibble %>%
	rename('value'='.')
chan.state <- rbind(nd.scored, nd.stable, nd.unstable) %>%
	mutate(key=str_remove_all(key, '(good_|stable_|_peer)'), key=str_replace(key, 'fail_min_median', 'low_median'), key=str_replace(key, 'fail_min_chan', 'low_chan'), key=str_replace(key, 'fail_max_disable_ratio', 'many_disabled_channels'), key=str_replace(key, 'fail_uptime_ratio', 'suboptimal_uptime')) 
nd.scores <- rbind(nd.scored, nd.stable) %>%
	filter(key=='score') %>%
	dplyr::select(name, value) %>%
	mutate(value=as.numeric(value)) %>%
	rename('tweb.score'='value')

bos_fn <- list.files(path="data", pattern="^bos-[0-9]+.json")
bos_fn <- paste0('data/', bos_fn[length(bos_fn)])
bos.list <- fromJSON(bos_fn, flatten=TRUE)
bos <- bos.list$scores %>%
	dplyr::select(public_key, score) %>%
	rename(c('name'='public_key', 'value'='score')) %>%
	mutate(key='bos') %>%
	as_tibble
chan.blocktimes <- read_tsv('data/chan-blockheights.tsv', col_names=FALSE) %>%
	rename(c('chan_point'='X1', 'blocktime'='X2'))
self <- '0382b31dcff337311bf919411c5073c9c9a129890993f94f4a16eaaeffd91c7788' # for testing

# fetch nodes
nodes <- graph$nodes %>%
	unnest(addresses) %>%
	dplyr::select(pub_key, alias, addr) %>%
	mutate(net=ifelse(grepl("::", addr), "ipv6", ifelse(!grepl("onion", addr), "ipv4", ifelse(str_length(addr)>30, "onion_v3", "onion_v2")))) %>%
	complete(pub_key, nesting(net)) %>%
	dplyr::group_by(pub_key) %>%
	arrange(alias) %>%
	mutate(alias=alias[1]) %>%
	pivot_wider(values_from=addr, names_from=net) %>%
	mutate(ipv4=as.character(ipv4), ipv6=as.character(ipv6), onion_v2=as.character(onion_v2), onion_v3=as.character(onion_v3)) %>%
	arrange(pub_key)
last_update <- graph$nodes %>% dplyr::select(pub_key, last_update) %>% as_tibble %>% mutate(last_update=as.numeric(ss.time - as_datetime(last_update), unit='days'))
nodes <- left_join(nodes, last_update)
# fetch links
edges <- graph$edges %>%
	mutate(capacity=as.numeric(capacity), node1_policy.time_lock_delta=as.numeric(node1_policy.time_lock_delta), node1_policy.fee_base_msat=as.numeric(node1_policy.fee_base_msat), node1_policy.fee_rate_milli_msat=as.numeric(node1_policy.fee_rate_milli_msat), node2_policy.time_lock_delta=as.numeric(node2_policy.time_lock_delta), node2_policy.fee_base_msat=as.numeric(node2_policy.fee_base_msat), node2_policy.fee_rate_milli_msat=as.numeric(node2_policy.fee_rate_milli_msat))%>%
	separate(chan_point, sep=':', into='chan_point', extra='drop') %>%
	left_join(chan.blocktimes, by='chan_point') %>%
	mutate(age=as.numeric(ss.time - as_datetime(blocktime), unit='days'))
n1 <- edges %>%
	dplyr::select(node1_pub, capacity) %>%
	rename(c('name'='node1_pub'))
n2 <- edges %>%
	dplyr::select(node2_pub, capacity) %>%
	rename(c('name'='node2_pub'))
# fetch total capacity per node
capacity <- rbind(n1, n2) %>%
	dplyr::group_by(name) %>%
	summarise(tot.capacity=sum(capacity), num.channels=n(), avg.capacity=mean(capacity), med.capacity=median(capacity))
# fetch number of dead channels, i.e., >14 days since last update
u1 <- edges %>%
	dplyr::group_by(node1_pub) %>%
	mutate(diff=as.numeric(ss.time - as_datetime(last_update), unit='days'), state=ifelse(diff>7, "inact.channels", "act.channels")) %>%
	dplyr::select(node1_pub, state) %>%
	rename(c('name'='node1_pub'))
u2 <- edges %>%
	dplyr::group_by(node2_pub) %>%
	mutate(diff=as.numeric(ss.time - as_datetime(last_update), unit='days'), state=ifelse(diff>7, "inact.channels", "act.channels")) %>%
	dplyr::select(node2_pub, state) %>%
	rename(c('name'='node2_pub'))
chanstates <- rbind(u1, u2) %>%
	group_by(name, state) %>%
	summarise(count=n()) %>%
	pivot_wider(names_from=state, values_from=count, values_fill=0)
# compute node approximate age
age1 <- edges %>%
	dplyr::select(node1_pub, age) %>%
	rename(c('name'='node1_pub'))
age2 <- edges %>%
	dplyr::select(node2_pub, age) %>%
	rename(c('name'='node2_pub'))
node.age <- rbind(age1, age2) %>%
	group_by(name) %>%
	filter(age==max(age, na.rm=TRUE)) %>%
	ungroup %>%
	distinct(name, .keep_all=TRUE)
# pull out node fees and compute the averages
f1 <- edges %>%
	group_by(node1_pub) %>%
	mutate(node1.mean.basefee=mean(node1_policy.fee_base_msat, na.rm=TRUE)) %>%
	dplyr::select(node1_pub, node1_policy.time_lock_delta, node1_policy.fee_base_msat, node1_policy.fee_rate_milli_msat) %>%
	mutate(node1_policy.time_lock_delta=ifelse(is.finite(node1_policy.time_lock_delta), node1_policy.time_lock_delta, NA), node1_policy.fee_base_msat=ifelse(is.finite(node1_policy.fee_base_msat), node1_policy.fee_base_msat, NA), node1_policy.fee_rate_milli_msat=ifelse(is.finite(node1_policy.fee_rate_milli_msat), node1_policy.fee_rate_milli_msat, NA))
names(f1) <- c('name', 'delta', 'base.msat', 'rate.ppm')
f2 <- edges %>%
	group_by(node2_pub) %>%
	mutate(node2.mean.basefee=mean(node2_policy.fee_base_msat, na.rm=TRUE)) %>%
	dplyr::select(node2_pub, node2_policy.time_lock_delta, node2_policy.fee_base_msat, node2_policy.fee_rate_milli_msat) %>%
	mutate(node2_policy.time_lock_delta=ifelse(is.finite(node2_policy.time_lock_delta), node2_policy.time_lock_delta, NA), node2_policy.fee_base_msat=ifelse(is.finite(node2_policy.fee_base_msat), node2_policy.fee_base_msat, NA), node2_policy.fee_rate_milli_msat=ifelse(is.finite(node2_policy.fee_rate_milli_msat), node2_policy.fee_rate_milli_msat, NA))
names(f2) <- c('name', 'delta', 'base.msat', 'rate.ppm')
fees <- rbind(f1, f2) %>%
	group_by(name) %>%
	summarise(mean.delta=mean(as.numeric(delta), na.rm=TRUE), mean.base.msat=mean(base.msat, na.rm=TRUE), mean.rate.ppm=mean(rate.ppm, na.rm=TRUE), median.base.msat=median(base.msat, na.rm=TRUE), median.rate.ppm=median(rate.ppm, na.rm=TRUE))
# build graph
g <- edges %>%
	dplyr::select(node1_pub, node2_pub, capacity, age, last_update, node1_policy.fee_base_msat, node1_policy.fee_rate_milli_msat, node2_policy.fee_base_msat, node2_policy.fee_rate_milli_msat) %>%
	mutate(last_update=as.numeric(as_datetime(max(last_update, na.rm=TRUE)) - as_datetime(last_update), units='days')) %>%
	rename(c('from_base_fee'='node1_policy.fee_base_msat', 'from_fee_rate'='node1_policy.fee_rate_milli_msat', 'to_base_fee'='node2_policy.fee_base_msat', 'to_fee_rate'='node2_policy.fee_rate_milli_msat')) %>%
	graph.data.frame(directed=TRUE) %>%
	as_tbl_graph
# join node information
g <- left_join(g, nodes, by=c('name'='pub_key'))
# join capacity
g <- left_join(g, capacity, by='name')
# add node ages
g <- left_join(g, node.age, by='name')
# join the fee values
g <- left_join(g, fees, by='name')
# join channel activity
g <- left_join(g, chanstates, by='name')
# find shortest paths between a given node and every other node to fetch
# prune the smallest disconnected subgraphs
all.subgraphs <- component.dist(asNetwork(g), connected='weak')
g <- delete_vertices(g, which(all.subgraphs$membership>1)) %>%
	as_tbl_graph %>%
	mutate(id=row_number())

# heuristics to speed up centrality measures
# ignore nodes with >50% inactive channels, total capacity <1e5 (q1) and only 1 channel (q1)
heuristics <- g %>%
	as_tibble %>%
	dplyr::select(tot.capacity, num.channels) %>%
	summarise(q1capacity=quantile(tot.capacity, 0.25), q1num.channels=quantile(num.channels, 0.25))
g_heur <- g %>%
	filter(act.channels>0, tot.capacity>heuristics$q1capacity, num.channels>heuristics$q1num.channels)
g_heur_ids <- g_heur %>%
	as_tibble %>%
	pull(id)
g_betw <- centr_betw(g_heur, directed=FALSE)
g_clo <- centr_clo(g_heur, mode='all')
g_eigen <- centr_eigen(g_heur)
# capacity weighted betweenness
w <- g_heur %>% activate(edges) %>% pull(capacity)
w <- 1/(w/1e8)
g_betw_w <- igraph::betweenness(g_heur, directed=FALSE, weights=w) %>% enframe %>% rename('cent.between.weight'='value')
g_clo_w <- igraph::closeness(g_heur, weights=w) %>% enframe %>% rename('cent.close.weight'='value')
g_eigen_w <- igraph::eigen_centrality(g_heur, directed=FALSE, weights=1/w)$vector %>% enframe %>% rename('cent.eigen.weight'='value')

g_cent_summ <- cbind(g_heur_ids, g_betw$res, g_clo$res, g_eigen$vector) %>%
	as_tibble %>%
	rename(c('id'='g_heur_ids', 'cent.between'='V2', 'cent.close'='V3', 'cent.eigen'='V4'))
g <- left_join(g, g_cent_summ, by='id')
g <- left_join(g, g_betw_w) %>% left_join(., g_clo_w) %>% left_join(., g_eigen_w)
# compute ranks for centrality scores
g <- g %>%
	mutate(cent.between.rank=rank(-cent.between, ties.method='first'), cent.eigen.rank=rank(-cent.eigen, ties.method='first'), cent.close.rank=rank(-cent.close, ties.method='first'), cent.between.weight.rank=rank(-cent.between.weight, ties.method='first'), cent.close.weight.rank=rank(-cent.close.weight, ties.method='first'), cent.eigen.weight.rank=rank(-cent.eigen.weight, ties.method='first'))

# add column with links to 1ml/amboss
g <- g %>%
	mutate(amboss=paste0("https://amboss.space/node/", name), oneml=paste0("https://1ml.com/node/", name))

# add bos scores
g <- left_join(g, bos %>%
	dplyr::select(name:value) %>%
	rename('bos'='value'), by='name')
# add terminal web scorse
g <- left_join(g, nd.scores)

node_ids <- paste(g %>% pull(alias), "-", g %>% pull(name))

chansim_filter_parms <- g %>%
	as_tibble %>%
	summarise(
		max.cap=round(max(tot.capacity)/1e8+1, 0),
		max.avg.capacity=max(avg.capacity)/1e8,
		max.num.channels=max(num.channels)+1,
		max.age=round(max(age)+1, 0),
		max.between=max(cent.between.rank, na.rm=TRUE),
		max.close=max(cent.close.rank, na.rm=TRUE),
		max.eigen=max(cent.eigen.rank, na.rm=TRUE))

chart_vars <- g %>%
	as_tibble %>%
	dplyr::select(tot.capacity:cent.eigen.weight.rank, bos, tweb.score, -id, -mean.delta) %>%
	names
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
table_vars <- g %>%
	as_tibble %>%
	dplyr::select(name, alias, tot.capacity:cent.eigen.weight.rank, bos, tweb.score, -id, -mean.delta) %>%
	names
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

nodes <- g %>% as_tibble
ids <- nodes %>% dplyr::select(name, id) %>% as_tibble
rev_edges <- g %>%
	activate(edges) %>%
	as_tibble %>%
	mutate(f=to, t=from, fbf=to_base_fee, tbf=from_base_fee, ff=to_fee_rate, tf=from_fee_rate) %>%
	dplyr::select(f, t, capacity, age, last_update, fbf, ff, tbf, tf) %>%
	rename(c('from'='f', 'to'='t', 'from_base_fee'='fbf', 'from_fee_rate'='ff', 'to_base_fee'='tbf', 'to_fee_rate'='tf'))
forw_edges <- g %>% activate(edges) %>% as_tibble
all_edges <- rbind(forw_edges, rev_edges)
chan.bals <- chan.state %>% filter(key=='inbound' | key=='outbound') %>% mutate(from.key=ifelse(key=='inbound', value, name), to.key=ifelse(key=='outbound', value, name)) %>% dplyr::select(from.key, to.key) %>% left_join(ids, by=c('from.key'='name')) %>% left_join(ids, by=c('to.key'='name')) %>% rename(c('from'='id.x', 'to'='id.y')) %>% mutate(balance='min1M') %>% dplyr::select(from, to, balance) %>% unique
all_edges <- left_join(all_edges, chan.bals)
node.redflags <- chan.state %>% filter(key=='low_median_capacity' | key=='low_chan_count' | key=='many_disabled_channels' | key=='suboptimal_uptime') %>% dplyr::select(name, key) %>% rename('state'='key') %>% unique
nodes <- left_join(nodes, node.redflags) %>% mutate(state=ifelse(is.na(last_update), "inactive", ifelse(last_update>14, "inactive", state))) %>% mutate(state=replace_na(state, "healthy")) 

g_dir <- tbl_graph(nodes, all_edges, directed=TRUE) %>%
	filter(act.channels>0) %>%
	activate(edges) %>%
	filter(!is.na(from_fee_rate), from_fee_rate<20e3, from_fee_rate>1) %>%
	activate(nodes) %>%
	mutate(id=row_number())
g_dir_node_ids <- paste(g_dir %>% pull(alias), "-", g_dir %>% pull(name))

save(g, g_dir, g_clo, g_betw, g_eigen, heuristics, table_vars, chart_vars, node_ids, g_dir_node_ids, chansim_filter_parms, file='graph.Rda')
