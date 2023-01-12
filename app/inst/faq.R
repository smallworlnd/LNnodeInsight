between_def <- "Betweeness centrality measures the number of shortest paths that pass through a node. A higher number of shortest paths a node has to any two other node in the network, the more likely they will be included in a route depending on the liquidity balance of each channel in the path."
between_def_title <- "Betweenness centrality"
between_box <- box(p(between_def), title=between_def_title, width=NULL, collapsible=TRUE)

close_def <- "Closeness/hopness centrality is a measure of how many hops it takes to reach any node on the network from a given node. The better the rank, the fewer the hops required to reach any and all nodes."
close_def_title <- "Closeness/hopness centrality"
close_box <- box(p(close_def), title=close_def_title, width=NULL, collapsible=TRUE)

eigen_def <- "Eigenvector/hubness centrality measures influence of a given node in the network. Higher ranks imply a well-connected node that is linked to other well-connected nodes. A lower eigenvector centrality could also imply a new and/or underserved node in the network."
eigen_def_title <- "Eigenvector/hubness centrality"
eigen_box <- box(p(eigen_def), title=eigen_def_title, width=NULL, collapsible=TRUE)

maxflow_def <- "Maximum flow is the highest amount of sats that can theoretically be pushed through a path if liquidity were 100% outbound. In reality, outbound across a path is likely 50% or less."
maxflow_def_title <- "Maximum liquidity flow"
maxflow_box <- box(p(maxflow_def), title=maxflow_def_title, width=NULL, collapsible=TRUE)

passive_def <- "This is a fee strategy that attempts to encourage higher liquidity volume and ideally bi-directional volume such that channel balances are passively maintained. The success of this strategy depends on multiple factors, and can often be difficult to achieve."
passive_def_title <- "Passive rebalancing"
passive_box <- box(p(passive_def), title=passive_def_title, width=NULL, collapsible=TRUE)

active_def <- "This is a fee strategy that typically observes less volume but recovers a higher proportion in fees that should cover the cost of rebalancing to maintain adequate channel balances."
active_def_title <- "Active rebalancing"
active_box <- box(p(active_def), title=active_def_title, width=NULL, collapsible=TRUE)

outbound_value_def <- "The value of outbound liquidity (i.e., your channel fees) is estimated by analysing the cost of potential payments in your node's neighborhood as well as to common payment destinations. Sustained volume at high fees imply high outbound value. High volume at relatively lower fees (i.e., lower percentile) suggests a given channel is underpriced, especially if liquidity moves in one direction only. Low volume at high fees suggests that a given channel may be overpriced. However, it may be worth maintaining higher fees at low volume depending on the size of a forwarded HTLC and the inbound & outbound channel pairs that forward the HTLC. Low volume at low fees suggests low demand through that channel. It may be worth considering reallocating liquidity elsewhere if low demand at low fees persists."
outbound_value_def_title <- "Outbound liquidity value"
outbound_value_box <- box(p(outbound_value_def), title=outbound_value_def_title, width=NULL, collapsible=TRUE)

sign_msg_box <- box(
	h4("lnd"), code('lncli signmessage "message to sign here"'),
	h4("c-lightning"), code('lightning-cli signmessage "message to sign here"'),
	h4("eclair"), code("eclair-cli signmessage --msg=$(echo -n 'message to sign here' | base64)"),
	h4("Ride The Lightning"), p("Navigate to Lightning > Sign/Verify > Copy"),
	h4("Thunderhub"), p("Navigate to Tools > Sign message > Copy"),
	title="Signing a message with your node's private keys", width=NULL, collapsible=TRUE)
