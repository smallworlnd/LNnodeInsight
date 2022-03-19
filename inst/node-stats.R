# needs nodeSelectUI, nodeListServer, getNodePubkey, modalActionButton,
# startButtonServer

ambossLinkUI <- function(id) {
	uiOutput(NS(id, 'amboss'))
}

nodeRanksUI <- function(id, rankId, desc) {
	valueBoxOutput(NS(id, rankId), width=3) %>% bs_embed_tooltip(title=desc, placement='top')
}

ranksButtonUI <- function(id) {
	actionBttn(
		inputId=NS(id, 'past_ranks_button'),
		label=paste('View historical ranks of this node for', as.numeric(Sys.getenv("PASTRANKS_MSAT"))/1e3, "sats"),
		style='fill',
		color='success',
		block=FALSE
	)
}

plotOutputUI <- function(id, plotTitle, plotId, plotType) {
	tabPanel(
		plotTitle,
		withSpinner(
			eval(
				parse(text=paste0(plotType, "(NS(id, \"", plotId, "\"))"))
			)
		),
		value=plotId,
		id=NS(id, paste0(plotId, '_tab')),
		width=NULL
	)
}

nodeStatHeaderUI <- function(id, txtId) {
	uiOutput(NS(id, txtId))
}

nodestatsUI <- function(id) {
	ns <- NS(id)
	fluidRow(
		useShinyjs(),
		rclipboardSetup(),
		column(8,
			fluidRow(
				box(background='yellow', width=12,
					nodeSelectUI(NS(id, "node_select"), listId="subject", lab='Enter pubkey/alias to view stats')
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
					rankId=c("cent.between.weight.rank", "cent.eigen.weight.rank", "cent.close.weight.rank", "bos.score"),
					desc=c(
						"Betweenness centrality but using channel capacities as weights.",
						"Eigenvector/hubness centrality but using channel capacities as weights.",
						"Closeness/hopness centrality but using channel capacities as weights.",
						"Rank according to the system designed by Alex Bosworth.")
				) %>% t %>% as.data.frame,
				function(x) nodeRanksUI(NS(id, "node_ranks"), rankId=x[1], desc=x[2])
			)
		),
		column(12, h3("Historical ranks")),
		column(12,
			conditionalPanel(
				"output.invoice_status == 'Paid'", ns=ns,
				box(
					width=12,
					plotOutputUI(NS(id, "past_ranks"), "Historical ranks", "past_ranks_plot", "plotlyOutput")
				)
			)
		),
		hr(),
		column(12, align='center',
			conditionalPanel(
				"output.invoice_status != 'Paid'", ns=ns,
				ranksButtonUI(NS(id, "show_past_ranks"))
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
						plotTitle=c("Fee comparison", "Peers of peers in common", "Terminal Web stats", "Peer network"),
						plotId=paste0(c("node_vs_peer_fees", "peer_overlap", "peer_ranks", "localnet"), "_plot"),
						plotType=c(rep("plotlyOutput", 3), "forceNetworkOutput")) %>% t %>% as.data.frame,
					function(x) plotOutputUI(NS(id, "peer_tab_selected"), plotTitle=x[1], plotId=x[2], plotType=x[3])
				) %>% unname
			)
		)
	)
}

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

pastRankServer <- function(id, stats) {
	moduleServer(id, function(input, output, session) {
		output$past_ranks_plot <- renderPlotly({
			req(stats$node)
			bos <- pool %>% tbl('bos') %>%
				filter(pubkey==!!stats$node$pubkey) %>%
				arrange(time) %>%
				as_tibble
			nd <- pool %>% tbl('nd') %>%
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
			if (nrow(bos) > 0) {
				plt <- plt %>% add_trace(data=bos, x=~time, y=~rank, name="BOS rank", type='scatter', mode='lines')
			}
			if (nrow(nd) > 0) {
				plt <- plt %>% add_trace(data=nd, x=~time, y=~rank, name="Terminal Web rank", type='scatter', mode='lines')
			}
			plt
		})
	})
}

nodevsLNServer <- function(id, plotTitle, plotId, xvar, stats) {
	moduleServer(id, function(input, output, session) {
		output[[plotId]] <- renderPlotly({
			req(stats$historical)
			stats$ln %>%
				plot_ly(x=~time, y=~eval(parse(text=xvar)), type='scatter', mode='lines', name='LN average') %>%
					add_trace(data=stats$historical, y=~eval(parse(text=xvar)), type='scatter', mode='lines', name=stats$node$alias) %>%
					layout(hovermode='x', xaxis=list(title="Date", tickangle=45), yaxis=list(title=plotTitle))
		})
	})
}

peerFeeServer <- function(id, graph=undir_graph, stats) {
	moduleServer(id, function(input, output, session) {
		output$node_vs_peer_fees_plot <- renderPlotly({
			req(stats$node)
			fees <- fetch_peer_fees(pubkey=stats$node$pubkey) %>%
				mutate(
					diff=ifelse(
						subject_fee>peer_fee, "Outbound fee is more than the inbound fee", ifelse(
							subject_fee==peer_fee, "Outbound is the same as inbound", "Outbound fee is less than the inbound fee"))) %>%
				arrange(diff) %>%
				mutate(diff=factor(diff))
			min_fee <- fees %>% dplyr::select(subject_fee, peer_fee) %>% unlist %>% min
			max_fee <- fees %>% dplyr::select(subject_fee, peer_fee) %>% unlist %>% max
			plt <- ggplot(fees, aes(y=subject_fee, x=peer_fee, text=paste0("Outgoing fee: ", subject_fee, "\n", peer_alias, " fee:", peer_fee), color=as.factor(diff), size=9, alpha=0.8)) +
				geom_point() +
				geom_abline(intercept=0, slope=1, linetype='dashed', alpha=0.4) +
				scale_color_manual(values = c("dodgerblue3", "orange", "darkgray")) +
				xlim(min_fee, max_fee) +
				ylim(min_fee, max_fee) +
				theme_minimal()
			ggplotly(plt, tooltip="text") %>%
				layout(
					xaxis=list(title="Incoming peer fee (ppm)", size=10),
					yaxis=list(title="Outgoing fee (ppm)", size=10),
					legend=list(orientation='v', x=-0.15, y=-0.15))
		})
	})
}

peerOverlapServer <- function(id, graph=undir_graph, stats) {
	moduleServer(id, function(input, output, session) {
		output$peer_overlap_plot <- renderPlotly({
			req(stats$node)
			common_peers <- fetch_peer_overlaps(pubkey=stats$node$pubkey)
			plot_ly(common_peers, x=~peers, y=~num_common_peers, type='scatter', mode='lines+markers') %>%
				layout(
					hovermode='x',
					xaxis=list(title="Peer alias", tickangle=45, categoryorder="array", categoryarray=common_peers$peers),
					yaxis=list(title="Number of peers in common")
				)
		})
	})
}

peerRankServer <- function(id, stats) {
	moduleServer(id, function(input, output, session) {
		output$peer_ranks_plot <- renderPlotly({
			dat <- pool %>% tbl('nd') %>%
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

nodeStatHeader <- function(id, headerId, stats, activeTxt, inactiveTxt) {
	moduleServer(id, function(input, output, session) {
		output[[headerId]] <- renderUI({
			txt <- ifelse(!is.null(stats$node), paste(stats$node$alias, activeTxt), inactiveTxt)
		})
	})
}

nodestatsServer <- function(id) {
	moduleServer(id, function(input, output, session) {
		nodeListServer("node_select", listId="subject")
		pubkey <- getNodePubkey("node_select", "subject")
		ambossLinkServer("link_to_amboss_page", pubkey)
		stats <- reactiveValues()

		observeEvent(pubkey(), {
			req(pubkey())
			stats$node <- nodes_current %>% filter(pubkey==pubkey())
			subject_nd <- nd_current %>%
				filter(pubkey==pubkey()) %>%
				dplyr::select(pubkey, score, rank) %>%
				rename(c('nd.score'='score', 'nd.rank'='rank'))
			subject_bos <- bos_current %>%
				filter(pubkey==pubkey()) %>%
				rename(c('bos.score'='score')) %>%
				dplyr::select(-time)
			stats$node <- left_join(stats$node, subject_nd, by='pubkey') %>%
				left_join(., subject_bos, by='pubkey')
			peer_ids <- adjacent_vertices(undir_graph, fetch_id(pubkey=pubkey()), mode='all') %>% unlist
			stats$peers <- undir_graph %>% as_tibble %>% filter(id %in% peer_ids) %>% mutate(tot.capacity=tot.capacity/1e8)
			stats$historical <- pool %>% tbl("nodes_historical") %>%
				filter(pubkey==!!pubkey()) %>%
				as_tibble %>%
				arrange(time)
			stats$ln <- pool %>% tbl("nodes_historical") %>%
				filter(mean.rate.ppm<20e3) %>%
				group_by(time) %>%
				summarise(
					tot.capacity=mean(tot.capacity, na.rm=TRUE),
					mean.rate.ppm=mean(mean.rate.ppm, na.rm=TRUE),
					avg.capacity=mean(avg.capacity, na.rm=TRUE)) %>%
				arrange(time) %>%
				as_tibble
		}, ignoreNULL=TRUE)
		lapply(
			data.frame(
				rankId=c(
					"cent.between.rank", "cent.eigen.rank", "cent.close.rank", "nd.rank",
					"cent.between.weight.rank", "cent.eigen.weight.rank", "cent.close.weight.rank", "bos.score"
				),
				subtitle=c(
					"Betweenness rank", "Eigenvector/hubness rank", "Closeness/hopness rank", "Terminal rank",
					"Capacity-weighted betweenness rank", "Capacity-weighted eigenvector/hubness rank", "Capacity-weighted closeness/hopness rank", "BOS score"
				)
			) %>% t %>% as.data.frame,
			function(x) nodeRankServer("node_ranks", x[1], x[2], stats)
		)
		lapply(
			data.frame(
				plotTitle=c("Total capacity (sat)", "Average fee rate (ppm)", "Average channel size (sat)"),
				plotId=paste0(c("cap_change", "fee_change", "chansize_change"), "_plot"),
				xvar=c("tot.capacity", "mean.rate.ppm", "avg.capacity")
			) %>% t %>% as.data.frame,
			function(x) nodevsLNServer("evol_tab_selected", x[1], x[2], x[3], stats)
		)
		peerFeeServer("peer_tab_selected", undir_graph, stats)
		peerOverlapServer("peer_tab_selected", undir_graph, stats)
		peerRankServer("peer_tab_selected", stats)
		localNetServer("peer_tab_selected", customjs, undir_graph, stats)
		nodeStatHeader("section_headers", "node_stats", stats, "vs the Network", "Node vs the Network")
		nodeStatHeader("section_headers", "peer_compare", stats, "peers", "Node peers")
		
		past_ranks_button <- startButtonServer("show_past_ranks", buttonId="past_ranks_button")

		invoice_status <- reactiveVal("Unpaid")
		output$invoice_status <- reactive({invoice_status()})

		invoice <- invoiceHandlingServer(
			"past_ranks",
			reactive_trigger=past_ranks_button,
			inv_fetch_url=Sys.getenv("STORE_URL"),
			inv_amt=Sys.getenv("PASTRANKS_MSAT"),
			inv_desc="historical ranks")

		observeEvent(invoice(), {
			req(invoice() == "Paid")
			invoice_status(invoice())
		})

		observeEvent(pubkey(), {
			invoice_status("Unpaid")
			pastRankServer("past_ranks", NULL)
		})

		observeEvent(invoice_status(), {
			req(invoice_status() == "Paid")
			pastRankServer("past_ranks", stats)
		})

		outputOptions(output, "invoice_status", suspendWhenHidden=FALSE)
	})
}

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
	server <- function(input, output, session) {
		nodestatsServer('x')
	}
	shinyApp(ui, server)
  
}
