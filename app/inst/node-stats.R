source("inst/shiny-common.R", local=TRUE)
source('inst/invoice-mgmt.R', local=TRUE)

#' modified networkd3 js
#' 
#' hides all links by default and only reveals on mouse over to avoid UI
#' overload
customjs <- read_file("js/networkd3.js")

#' amboss link UI element
#'
#' UI displays amboss logo hyperlinked to website by default, or node page if a
#' node is selected in \link{nodeSelectUI}
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return returns amboss logo UI element with hyperlink
#' @export
ambossLinkUI <- function(id) {
	uiOutput(NS(id, 'amboss'))
}

#' node rank UI element
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param rankId rankId string, possibilities are 'cent.between.rank',
#' 'cent.eigen.rank', 'cent.close.rank', 'nd.rank', 'cent'between.weight.rank',
#' 'cent.eigen.weight.rank', 'cent.close.weight.rank'
#' @param desc description of rankId as a character string
#' @return returns valuebox UI element for the given rank of a node (if selected
#' in \link{nodeSelectUI}
#' @export
nodeRanksUI <- function(id, rankId, desc) {
	valueBoxOutput(NS(id, rankId), width=3) %>% bs_embed_tooltip(title=desc, placement='top')
}

#' historical node rank button UI element
#'
#' UI button element linked with invoice handling
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return returns valuebox UI element for the given rank of a node (if selected
#' in \link{nodeSelectUI}
#' @export
ranksButtonUI <- function(id) {
	actionBttn(
		inputId=NS(id, 'ranks_bal_button'),
		label=paste('View historical ranks of this node for', as.numeric(Sys.getenv("PASTRANKS_MSAT"))/1e3, "sats"),
		style='fill',
		color='success',
		block=FALSE
	)
}

#' html header UI element
#'
#' used for sectioning the page
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param txtId ID of the text server element
#' @return returns simple HTML header UI element
#' @export
nodeStatHeaderUI <- function(id, txtId) {
	uiOutput(NS(id, txtId))
}


#' main layout UI for various app elements
#'
#' byoc page layout
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @export
nodestatsUI <- function(id) {
	ns <- NS(id)
	fluidRow(
		useShinyjs(),
		rclipboardSetup(),
		column(8,
			fluidRow(
				box(nodeSelectUI(NS(id, "node_select"), listId="subject", lab='Enter pubkey/alias to view stats'),
					background='yellow', width=12
				),
			)
		),
		column(4,
			tags$br(),
			tags$div("Also see node stats on"),
			ambossLinkUI(NS(id, "link_to_amboss_page"))
		),
		column(12,
			h3('Current ranks')
		),
		column(12,
			lapply(
				data.frame(
					rankId=c("cent.between.rank", "cent.eigen.rank", "cent.close.rank", "nd.rank"),
					desc=c(
						"Betweenness centrality measures how many shortest paths a node sits in between any two other nodes. Higher ranking nodes tend to be in more shortest paths between other nodes and are thus more likely to be in a potential route.",
						"Eigenvector/hubness centrality is a node's influence in the network. Higher ranking nodes tend to have more channels, and are also connected to other high ranking nodes who themselves have many channels.",
						"Closeness/hopness centrality measures the distance from a node to any other in the network. Higher ranking nodes have to make fewer hops to reach any other node on the network.",
						"Rank according to the system designed by Lightning Labs.")
				) %>% t %>% as.data.frame,
				function(x) nodeRanksUI(NS(id, "node_ranks"), rankId=x[1], desc=x[2])
			)
		),
		column(12,
			lapply(
				data.frame(
					rankId=c("cent.between.weight.rank", "cent.eigen.weight.rank", "cent.close.weight.rank"),
					desc=c(
						"Betweenness centrality but using channel capacities as weights.",
						"Eigenvector/hubness centrality but using channel capacities as weights.",
						"Closeness/hopness centrality but using channel capacities as weights.")
				) %>% t %>% as.data.frame,
				function(x) nodeRanksUI(NS(id, "node_ranks"), rankId=x[1], desc=x[2])
			)
		),
		column(6, h3("Historical ranks and node liquidity distribution") %>% bs_embed_tooltip(title="Liquidity distributions typically available for nodes with at least 10 channels but some exceptions exist")),
		conditionalPanel(
			"output.invoice_status == 'Paid'", ns=ns,
			do.call(tabBox,
				c(id=NS(id, 'ranks_bal'), side='left', width=12,
					lapply(
						data.frame(
							tabId=c("ranks_tab", "liq_tab"),
							plotTitle=c("Historical ranks", "Node liquidity estimate"),
							plotId=paste(c("past_ranks", "node_liq"), "plot", sep="_"),
							plotType=rep("plotlyOutput", 2)) %>% t %>% as.data.frame,
						function(x)
							plotOutputUI(NS(id, x[1]), plotTitle=x[2], plotId=x[3], plotType=x[4])
					) %>% unname
				)
			)
		),
		hr(),
		column(12, align='center',
			conditionalPanel(
				"output.invoice_status != 'Paid'", ns=ns,
				ranksButtonUI(NS(id, "show_ranks_bal"))
			)
		),
		column(12, h3(nodeStatHeaderUI(NS(id, 'section_headers'), "node_stats"))),
		do.call(tabBox,
			c(id=NS(id, 'node_evol'), side='left', width=12,
				lapply(
					data.frame(
						plotTitle=c('Total capacity', 'Channel capacity', 'Fees'),
						plotId=paste0(c("cap_change", "chansize_change", "fee_change"), "_plot"),
						plotType=rep("plotlyOutput", 3)) %>% t %>% as.data.frame,
					function(x) plotOutputUI(NS(id, "evol_tab_selected"), plotTitle=x[1], plotId=x[2], plotType=x[3])
				) %>% unname
			)
		),
		column(12,
			h3(nodeStatHeaderUI(NS(id, 'section_headers'), "peer_compare"))
		),
		do.call(tabBox,
			c(id=NS(id, 'peer_tab'), side='left', width=12,
				lapply(
					data.frame(
						plotTitle=c("Fee comparison", "Peers of peers in common", "Terminal Web stats", "Peer network", "Triangles"),
						plotId=paste0(c("node_vs_peer_fees", "peer_overlap", "peer_ranks", "localnet", "triangles"), "_plot"),
						plotType=c(rep("plotlyOutput", 3), "forceNetworkOutput", "dataTableOutput")) %>% t %>% as.data.frame,
					function(x) plotOutputUI(NS(id, "peer_tab_selected"), plotTitle=x[1], plotId=x[2], plotType=x[3])
				) %>% unname
			)
		)
	)
}

#' amboss link generator
#'
#' @param id An ID string that corresponds with the ID used to call
#' \link{ambossLinkUI}
#' @param pubkey pubkey of the selected node with which to build hyperlink to
#' amboss
#' @return returns hyperlinked logo output element
#' @export
ambossLinkServer <- function(id, pubkey) {
	moduleServer(id, function(input, output, session) {
		output$amboss <- renderUI({
			if (pubkey() != "") {
				link <- paste0("https://amboss.space/node/", pubkey())
			} else {
				link <- "https://amboss.space"
			}
			tags$a(
				href=link, 
				tags$img(src="www/AmbossLogo.png", 
					width="125px",
					height="100%"),
				target="_blank")

		})
	})
}

#' node rank server
#'
#' used for generating output element for the ranks of the node selected with
#' \link{nodeSelectUI}
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param rankId rankId string, possibilities are 'cent.between.rank',
#' 'cent.eigen.rank', 'cent.close.rank', 'nd.rank', 'cent'between.weight.rank',
#' 'cent.eigen.weight.rank', 'cent.close.weight.rank'
#' @param desc description of rankId as a character string
#' @param node_stats_reactive reactive element of a node's pubkey
#' @return returns valuebox server output for a given rank metric
#' @export
nodeRankServer <- function(id, rankId, desc, node_stats_reactive) {
	moduleServer(id, function(input, output, session) {
		ranks <- reactiveValues(node=NULL)
		observeEvent(node_stats_reactive$node, {
			ranks$node <- node_stats_reactive$node
		})
		output[[rankId]] <- renderValueBox({
			if (!is.null(ranks$node)) {
				val <- ranks$node %>%
					pull(eval(parse(text=rankId))) %>%
					round(0) %>%
					format(scientific=FALSE) %>%
					prettyNum(big.mark=",")
			} else {
				val <- ''
			}
			valueBox(val, desc, color='blue')
		})
	})
}

#' historical node rank server
#'
#' generates an output element containing historical ranks for a given node
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param stats reactiveValues containing current and historical node and LN #' data
#' @return returns plotlyOutput element for node historical ranks
#' @export
pastRankServer <- function(id, stats, db=pool) {
	moduleServer(id, function(input, output, session) {
		output$past_ranks_plot <- renderPlotly({
			req(stats$node)
			nd <- tbl(db, 'nd') %>%
				filter(pubkey==!!stats$node$pubkey) %>%
				arrange(time) %>%
				as_tibble
			plt <- stats$historical %>%
				as_tibble %>%
				plot_ly(x=~time, y=~cent.between.rank, name="Betweenness", type='scatter', mode='lines', hovertemplate="%{y}") %>%
				add_trace(y=~cent.close.rank, name="Closeness/hopness") %>%
				add_trace(y=~cent.eigen.rank, name="Eigenvector/hubness") %>%
				add_trace(y=~cent.between.weight.rank, name="Capacity-weighted betweenness") %>%
				add_trace(y=~cent.close.weight.rank, name="Capacity-weighted closeness/hopness") %>%
				add_trace(y=~cent.eigen.weight.rank, name="Capacity-weighted eigenvector/hubness") %>%
				layout(yaxis=list(title="Rank", type='log'), xaxis=list(title="Date"), hovermode='x')
			if (nrow(nd) > 0) {
				plt <- plt %>% add_trace(data=nd, x=~time, y=~rank, name="Terminal Web rank", type='scatter', mode='lines')
			}
			plt
		})
	})
}

#' liquidity server
#'
#' display ribbon plot of liquidity distribution and low capacity channels
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param db db connectiona object
#' @return returns ggplotly output element
#' @export
liquidityServer <- function(id, stats, db=pool) {
	moduleServer(id, function(input, output, session) {
		output$node_liq_plot <- renderPlotly({
			req(stats$node)
			dat <- tbl(db, 'node_probes') %>%
				filter(from==!!stats$node$pubkey) %>%
				as_tibble
			plot_ly(data=dat, labels=~desc, values=~n, type="pie", hole=0.6, marker=list(colors=~color))
		})
	})
}

#' historical network-wide plot server
#'
#' generates an output element containing network-wide historical data on fees,
#' capacity and channel size
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param plotTitle y-axis label
#' @param plotId id of the plot output element, e.g., 'cap_change',
#' 'chansize_change' ,'fee_change' 
#' @param xvar variable to plot against time
#' @param stats reactiveValues containing current and historical node and LN #' data
#' @return returns plotlyOutput element for node historical capacity/fee/channel
#' size compared to the network
#' @export
nodevsLNServer <- function(id, plotTitle, plotId, xvar, stats) {
	moduleServer(id, function(input, output, session) {
		output[[plotId]] <- renderPlotly({
			req(stats$historical)
			if (xvar == "mean.rate.ppm") {
				stats$ln %>%
					plot_ly(x=~time, y=~eval(parse(text=paste0(xvar, ".out"))), type='scatter', mode='lines', name='LN outbound average') %>%
						add_trace(y=~eval(parse(text=paste0(xvar, ".in"))), type='scatter', mode='lines', name='LN inbound average') %>%
						add_trace(data=stats$historical, y=~eval(parse(text=paste0(xvar, ".out"))), type='scatter', mode='lines', name=paste(stats$node$alias, "outbound average")) %>%
						add_trace(data=stats$historical, y=~eval(parse(text=paste0(xvar, ".in"))), type='scatter', mode='lines', name=paste(stats$node$alias, "inbound average")) %>%
						layout(hovermode='x', xaxis=list(title="Date"), yaxis=list(title=plotTitle))
			} else {
				stats$ln %>%
					plot_ly(x=~time, y=~eval(parse(text=xvar)), type='scatter', mode='lines', name='LN average') %>%
						add_trace(data=stats$historical, y=~eval(parse(text=xvar)), type='scatter', mode='lines', name=stats$node$alias) %>%
						layout(hovermode='x', xaxis=list(title="Date"), yaxis=list(title=plotTitle))
			}
		})
	})
}

#' node vs peer fee server
#'
#' extracts a node's peer fees and generates a ggplotly output of a scatterplot
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param graph igraph/tbl_graph object from which to fetch fees from channels
#' @param stats reactiveValues containing current and historical node and LN #' data
#' @return returns ggplotly output element
#' @export
peerFeeServer <- function(id, graph=undir_graph, stats) {
	moduleServer(id, function(input, output, session) {
		output$node_vs_peer_fees_plot <- renderPlotly({
			req(stats$node)
			fees <- fetch_peer_fees(pubkey=stats$node$pubkey) %>%
				mutate(
					diff=ifelse(
						subject_fee>peer_fee, "Out fee > in fee", ifelse(
							subject_fee==peer_fee, "Out fee equals in fee", "Out fee < in fee"))) %>%
				arrange(diff) %>%
				mutate(diff=factor(diff))
			min_fee <- fees %>% dplyr::select(subject_fee, peer_fee) %>% unlist %>% min
			max_fee <- pmin(fees %>% dplyr::select(subject_fee, peer_fee) %>% filter(!is.na(subject_fee), !is.na(peer_fee)) %>% unlist %>% max, 10000)
			plt <- ggplot(fees, aes(y=subject_fee, x=peer_fee, text=paste0("Outgoing fee: ", subject_fee, "\n", peer_alias, " fee:", peer_fee), color=as.factor(diff), size=9, alpha=0.8)) +
				geom_point() +
				ggtitle("") +
				xlab("Incoming peer fee (ppm)") + ylab("Outgoing fee (ppm)") +
				geom_abline(intercept=0, slope=1, linetype='dashed', alpha=0.4) +
				scale_color_manual(values = c("dodgerblue3", "orange", "darkgray")) +
				xlim(min_fee, max_fee) +
				ylim(min_fee, max_fee) +
				theme_minimal() +
				guides(fill=guide_legend(title=NULL), alpha=guide_legend(title=NULL), color=guide_legend(title=NULL), size=guide_legend(title=NULL))
			ggplotly(plt, tooltip="text")
		})
	})
}

#' peers of peers in common server
#'
#' extracts a node's peers of peers to compute overlap numbers
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param graph igraph/tbl_graph object from which to fetch fees from channels
#' @param stats reactiveValues containing a node's historical stats
#' @return returns plotly output element to draw a plot of number of peers in
#' common
#' @export
peerOverlapServer <- function(id, graph=undir_graph, stats) {
	moduleServer(id, function(input, output, session) {
		output$peer_overlap_plot <- renderPlotly({
			req(stats$node)
			common_peers <- fetch_peer_overlaps(pubkey=stats$node$pubkey)
			plot_ly(common_peers, x=~peers, y=~num_common_peers, type='scatter', mode='lines+markers') %>%
				layout(
					hovermode='x',
					xaxis=list(title="Peer alias", categoryorder="array", categoryarray=common_peers$peers),
					yaxis=list(title="Number of peers in common")
				)
		})
	})
}

#' peer terminal rank server
#'
#' extracts the terminal ranks of a node's peers
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param stats reactiveValues containing a node's historical stats
#' @return returns plotly output element to draw the terminal rank of a node's
#' peers and corresponding terminal "status"
#' @export
peerRankServer <- function(id, stats, db=pool) {
	moduleServer(id, function(input, output, session) {
		output$peer_ranks_plot <- renderPlotly({
			dat <- tbl(db, 'nd') %>%
				filter(time==max(time) && pubkey %in% !!stats$peers$pubkey) %>%
				as_tibble %>%
				left_join(., stats$peers %>% dplyr::select(pubkey, alias) %>% as_tibble) %>%
				mutate(state=ifelse(
					state=="healthy", "Healthy", ifelse(
					state=="stable", "Stable", ifelse(
					state=="fail_min_median_capacity", "Median channel capacity less than 500k sat", ifelse(
					state=="fail_max_disable_ratio", "More than 25% of channels are disabled", ifelse(
					state=="fail_uptime_ratio", "Uptime less than 99.9%", ifelse(
					state=="fail_min_chan_count", "Node has less than 10 channels", state)))))))
			plt <- plot_ly(dat, x=~rank, y=~good_peers, color=~state,
				type='scatter', mode='markers', size=20, alpha=0.9, text=~alias,
				hovertemplate=paste0("%{text}\n", "Rank: %{x}\n", "Number of good peers: %{y}")) %>%
				layout(
					xaxis=list(title="Terminal Web rank", type='log'),
					yaxis=list(title="Number of good peers"),
					legend=list(orientation='h', y=-0.15))
			plt
		})
	})
}

#' networkd3 graph of a node
#'
#' plots an interactive graph of a node's neighborhood
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param customjs the custom js file adapted from networkd3 source
#' @param graph igraph/tbl_graph object from which to fetch neighborhood
#' @param stats reactiveValues containing current and historical node and LN #' data
#' @return returns plotly output element to draw the terminal rank of a node's
#' peers and corresponding terminal "status"
#' @export
localNetServer <- function(id, customjs=customjs, graph=undir_graph, stats) {
	moduleServer(id, function(input, output, session) {
		output$localnet_plot <- renderForceNetwork({
			req(stats$node)
			local_graph <- peer_graph(graph, stats$node$pubkey)
			node <- local_graph %>%
				mutate(id=row_number()-1) %>%
				as_tibble %>%
				mutate(group=ifelse(pubkey==stats$node$pubkey, 1, 2), nodesize=tot.capacity/1e8)
			links <- local_graph %>%
				activate(edges) %>%
				as_tibble %>%
				mutate(from=from-1, to=to-1) %>%
				arrange(from, to) %>%
				mutate(capacity=capacity/1e7)
			fn <- forceNetwork(
				Links=links, Nodes=node, Source='from', Target='to', Nodesize='nodesize',
				NodeID='alias', Group='group', opacity=1, fontSize=16, Value='capacity',
				bounded=FALSE, fontFamily='sans-serif', zoom=TRUE, charge=-1000, opacityNoHover=1,
				colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"))
			fn$x$nodes$pubkey <- node$pubkey
			fn$x$nodes$amboss <- paste0("https://amboss.space/node/", node$pubkey)
			onRender(fn, customjs)
		})
	})
}

#' peer triangle table
#'
#' shows all possible triangles between a node and its peers
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param graph igraph/tbl_graph object from which to fetch neighborhood
#' @param stats reactiveValues containing current and historical node and LN #' data
#' @return returns 3-column dataTableOutput object of all peer triangles
#' @export
peerTriangleServer <- function(id, graph=undir_graph, stats) {
	moduleServer(id, function(input, output, session) {
		output$triangles_plot <- renderDataTable({
			req(stats$node)
			fetch_node_triangles(graph, pubkey=stats$node$pubkey)
		})
	})
}

#' header UI server
#'
#' generates text for the header UI element
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param headerId header ID linked to a node
#' @param stats reactiveValues containing current and historical node and LN #' data
#' @param activeTxt string to show if a node is selected
#' @param inactiveTxt string to show if no node is selected
#' @return returns output element for header text
#' @export
nodeStatHeader <- function(id, headerId, stats, activeTxt, inactiveTxt) {
	moduleServer(id, function(input, output, session) {
		output[[headerId]] <- renderUI({
			txt <- ifelse(!is.null(stats$node), paste(stats$node$alias, activeTxt), inactiveTxt)
		})
	})
}

#' main app server
#'
#' module running node stat outputs
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns backend for the app UI
#' @export
nodestatsServer <- function(id, credentials, url_pubkey_search, ln_summary=ln_summary_stats, db=pool) {
	moduleServer(id, function(input, output, session) {
		# initialize the node list from which to select
		nodeListServer("node_select", listId="subject", default_selected=url_pubkey_search)
		pubkey <- getNodePubkey("node_select", "subject")
		ambossLinkServer("link_to_amboss_page", pubkey)
		stats <- reactiveValues()

		# on pubkey selection, compute relevant stats for that pubkey
		observeEvent(pubkey(), {
			req(pubkey())
			stats$node <- nodes_current %>% filter(pubkey==pubkey())
			subject_nd <- nd_current %>%
				filter(pubkey==pubkey()) %>%
				dplyr::select(pubkey, score, rank) %>%
				rename(c('nd.score'='score', 'nd.rank'='rank'))
			stats$node <- left_join(stats$node, subject_nd, by='pubkey')
			peer_ids <- adjacent_vertices(undir_graph, fetch_id(pubkey=pubkey()), mode='all') %>% unlist
			stats$peers <- undir_graph %>% as_tibble %>% filter(id %in% peer_ids) %>% mutate(tot.capacity=tot.capacity/1e8)
			stats$historical <- tbl(db, "nodes_historical") %>%
				filter(pubkey==!!pubkey()) %>%
				as_tibble %>%
				arrange(time)
			stats$ln <- ln_summary
		}, ignoreNULL=TRUE)
		# generate rank outputs for the selected node
		lapply(
			data.frame(
				rankId=c(
					"cent.between.rank", "cent.eigen.rank", "cent.close.rank", "nd.rank",
					"cent.between.weight.rank", "cent.eigen.weight.rank", "cent.close.weight.rank"
				),
				subtitle=c(
					"Betweenness rank", "Eigenvector/hubness rank", "Closeness/hopness rank", "Terminal rank",
					"Capacity-weighted betweenness rank", "Capacity-weighted eigenvector/hubness rank", "Capacity-weighted closeness/hopness rank"
				)
			) %>% t %>% as.data.frame,
			function(x) nodeRankServer("node_ranks", x[1], x[2], stats)
		)
		# generate capacity/fee/channel size outputs for the selected node
		lapply(
			data.frame(
				plotTitle=c("Total capacity (sat)", "Average fee rate (ppm)", "Average channel size (sat)"),
				plotId=paste0(c("cap_change", "fee_change", "chansize_change"), "_plot"),
				xvar=c("tot.capacity", "mean.rate.ppm", "avg.capacity")
			) %>% t %>% as.data.frame,
			function(x) nodevsLNServer("evol_tab_selected", x[1], x[2], x[3], stats)
		)
		# generate outputs for the remaining graph elements
		peerFeeServer("peer_tab_selected", undir_graph, stats)
		peerOverlapServer("peer_tab_selected", undir_graph, stats)
		peerRankServer("peer_tab_selected", stats)
		localNetServer("peer_tab_selected", customjs, undir_graph, stats)
		peerTriangleServer("peer_tab_selected", undir_graph, stats)

		# reactive headers
		nodeStatHeader("section_headers", "node_stats", stats, "vs the Network", "Node vs the Network")
		nodeStatHeader("section_headers", "peer_compare", stats, "peers", "Node peers")
		
		ranks_bal_button <- startButtonServer("show_ranks_bal", buttonId="ranks_bal_button")

		# initialize invoice status
		invoice_status <- reactiveVal("Unpaid")
		output$invoice_status <- reactive({invoice_status()})

		# generate an invoice on button click
		invoice <- invoiceHandlingServer(
			"ranks_bal",
			reactive_trigger=ranks_bal_button,
			inv_fetch_url=Sys.getenv("STORE_URL"),
			inv_amt=Sys.getenv("PASTRANKS_MSAT"),
			display_desc=paste("Please pay", as.numeric(Sys.getenv("PASTRANKS_MSAT"))/1e3, "sats to view results."),
			inv_desc="historical ranks")

		# modify invoice status reactiveVal to "Paid" if the
		# invoice gets paid
		observeEvent(invoice(), {
			req(invoice() == "Paid")
			invoice_status(invoice())
		})

		# reset the historical rank graph if a new node is selected
		observeEvent(pubkey(), {
			invoice_status("Unpaid")
			pastRankServer("ranks_tab", NULL)
			liquidityServer("liq_tab", NULL)
		})

		# if the invoice gets paid, then show historical ranks
		observeEvent(invoice_status(), {
			req(invoice_status() == "Paid")
			pastRankServer("ranks_tab", stats)
			liquidityServer("liq_tab", stats)
		})

		# send invoice status to the client side
		outputOptions(output, "invoice_status", suspendWhenHidden=FALSE)

		# determine if account is premium
		is_premium <- premiumAccountReactive("prem_account", credentials, db)
		# if account is premium, short-circuit button click/invoice creation
		# process by settting invoice status to always "paid"
		observe({
			req(is_premium() == "true")
			invoice_status("Paid")
			if (pubkey() == "") {
				pastRankServer("ranks_tab", NULL)
			} else {
				pastRankServer("ranks_tab", stats)
			}
		})
	})
}

#' node stats app standalone
#'
#' for dev/testing purposes
nodestatsApp <- function() {

	addResourcePath('www', 'www')
	ui <- function() {
		dashboardPage(
			dashboardHeader(title='Node Stats'),
			dashboardSidebar(),
			dashboardBody(nodestatsUI('x')),
			skin='yellow',
		)
	}
	credentials <- reactiveValues(
		info=data.frame(pubkey=test_pubkey, foo="bar"),
		user_auth=TRUE)
		#info=NULL,
		#user_auth=FALSE)
	server <- function(input, output, session) {
		nodestatsServer('x', reactive(credentials), NULL)
	}
	shinyApp(ui, server)
  
}
