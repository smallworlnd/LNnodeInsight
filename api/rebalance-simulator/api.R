if(Sys.getenv("PORT") == "") Sys.setenv(PORT = 8000)

#* Returns a list of the simulated graph, and new centrality metrics
#* @param subject_pubkey pubkey of the subject_pubkey node we're simulating channels
#* @param out_pubkey pubkey of the outgoing node
#* @param in_pubkey pubkey of the incoming node
#* @param max_samp maximum number of paths to sample
#* @post /rebal_res
#' @serializer tsv
path_flow_cost <- function(subject_pubkey="", out_pubkey, in_pubkey, max_samp=1000) {
	in_id <- fetch_id_from_pubkey(graph, in_pubkey)
	out_id <- fetch_id_from_pubkey(graph, out_pubkey)
	# figure out if in_pubkey has channel to subject_pubkey
	if (subject_pubkey != "" && are_adjacent(graph, fetch_id_from_pubkey(nodes, subject_pubkey), in_id)) {
		return_fee <- E(graph, path=data.frame(from=in_id, to=fetch_id_from_pubkey(nodes, subject_pubkey)))$from_fee_rate
	} else {
		# otherwise use median in_pubkey fee
		return_fee <- nodes %>% filter(pubkey==in_pubkey) %>% pull(median.rate.ppm)
	}
	# find a single shortest path
	dat <- build_path_flow_cost_dist(graph, out_id, in_id, return_fee)
	return(dat)
}
