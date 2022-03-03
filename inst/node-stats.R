nodeStatsUI <- function(id) {
	useShinyjs()
	fluidRow(
		column(8,
			fluidRow(
				box(selectizeInput(inputId="nodestats_subject", label='Enter pubkey/alias to view stats', choices=NULL, options=list(placeholder='Pubkey/alias')), background='yellow', width=12),
			)
		),
		column(4,
			tags$br(),
			tags$div("Also see node stats on"),
			uiOutput('ambossnodestats')
		)),
#				column(6,
#					uiOutput('table_vars')),
	h3('Current ranks'),
	fluidRow(
		valueBoxOutput('nodestats.cent.between', width=3) %>% bs_embed_tooltip(title="Betweenness centrality measures how many shortest paths a node sits in between any two other nodes. Higher ranking nodes tend to be in more shortest paths between other nodes and are thus more likely to be in a potential route.", placement='top'),
		valueBoxOutput('nodestats.cent.eigen', width=3) %>% bs_embed_tooltip(title="Eigenvector/hubness centrality is a node's influence in the network. Higher ranking nodes tend to have more channels, and are also connected to other high ranking nodes who themselves have many channels.", placement='top'),
		valueBoxOutput('nodestats.cent.close', width=3) %>% bs_embed_tooltip(title="Closeness/hopness centrality measures the distance from a node to any other in the network. Higher ranking nodes have to make fewer hops to reach any other node on the network.", placement='top'),
		valueBoxOutput('nodestats.nd', width=3) %>% bs_embed_tooltip(title="Rank according to the system designed by Lightning Labs.", placement='top')),
	fluidRow(
		valueBoxOutput('nodestats.cent.between.weight', width=3) %>% bs_embed_tooltip(title="Betweenness centrality but using channel capacities as weights.", placement='top'),
		valueBoxOutput('nodestats.cent.eigen.weight', width=3) %>% bs_embed_tooltip(title="Eigenvector/hubness centrality but using channel capacities as weights.", placement='top'),
		valueBoxOutput('nodestats.cent.close.weight', width=3) %>% bs_embed_tooltip(title="Closeness/hopness centrality but using channel capacities as weights.", placement='top'),
		valueBoxOutput('nodestats.bos', width=3) %>% bs_embed_tooltip(title="Rank according to the system designed by Alex Bosworth.", placement='top')),
	h3("Historical ranks"),
	conditionalPanel(
		condition="input.nodestats_subject != ''",
		conditionalPanel(
			condition="output.histranks_inv_settled==TRUE",
			withSpinner(plotlyOutput("rank_change", width=NULL)),
		),
		hr(),
		column(12, align='center',
			actionBttn(inputId='show_hist_ranks', label=paste('View historical ranks for', as.numeric(histranks_msat)/1e3, 'sats'), style='fill', color='success', block=FALSE)),
	),
	h3(uiOutput('lncompare')),
	fluidRow(
		tabBox(id='nodestats_cap_change_tab', side='left', selected='node_cap_change', width=12,
			tabPanel('Total capacity', withSpinner(plotlyOutput("node_cap_change")), value='node_cap_change', id='node_cap_change', width=NULL),
			tabPanel('Channel capacity ', withSpinner(plotlyOutput("node_chansize_change")), value='node_chansize_change', id='node_chansize_change', width=NULL),
			tabPanel('Fees ', withSpinner(plotlyOutput("node_fee_change")), value='node_fee_change', id='node_fee_change', width=NULL),
		)
	),
	h3(uiOutput('peercompare')),
	fluidRow(
		tabBox(id='peer_chancap_change_tab', side='left', selected='fee_comp', width=12,
			tabPanel('Fee comparison', withSpinner(plotlyOutput("node_vs_peer_fees", height="750px")), value='fee_comp', id='fee_comp', width=NULL),
			tabPanel('Peers of peers in common', withSpinner(plotlyOutput("peer_overlap", height="750px")), value='peer_overlap', id='peer_overlap', width=NULL),
			tabPanel('Terminal Web stats', withSpinner(plotlyOutput("peer_ranks", height="750px")), value='peer_ranks', id='peer_ranks', width=NULL),
			tabPanel('Peer network', withSpinner(forceNetworkOutput("net", height="750px")), value='forcenet', id='forcenet', width=NULL),
		)
	),
}

nodeStatsServer <- function(id) {
	moduleServer(id, function(input, output, session) {
		# node statistics/graph rendering
		output$nodestats.cent.between <- renderValueBox({
			if (!is.null(nodestats$data$pubkey)) {
				cent.between.rank <- nodestats$data$cent.between.rank
				val <- prettyNum(cent.between.rank, big.mark=',')
			} else {
				val <- ''
			}
			valueBox(val, "Betweenness rank", color='blue')
		})
		output$nodestats.cent.between.weight <- renderValueBox({
			if (!is.null(nodestats$data$pubkey)) {
				cent.between.weight.rank <- nodestats$data$cent.between.weight.rank
				val <- prettyNum(cent.between.weight.rank, big.mark=',')
			} else {
				val <- ''
			}
			valueBox(val, "Capacity-weighted betweenness rank", color='blue')
		})
		output$nodestats.cent.eigen <- renderValueBox({
			if (!is.null(nodestats$data$pubkey)) {
				cent.eigen.rank <- nodestats$data$cent.eigen.rank
				val <- prettyNum(cent.eigen.rank, big.mark=',')
			} else {
				val <- ''
			}
			valueBox(val, "Eigenvector/hubness rank", color='blue')
		})
		output$nodestats.cent.eigen.weight <- renderValueBox({
			if (!is.null(nodestats$data$pubkey)) {
				cent.eigen.weight.rank <- nodestats$data$cent.eigen.weight.rank
				val <- prettyNum(cent.eigen.weight.rank, big.mark=',')
			} else {
				val <- ''
			}
			valueBox(val, "Capacity-weighted eigenvector/hubness rank", color='blue')
		})
		output$nodestats.cent.close <- renderValueBox({
			if (!is.null(nodestats$data$pubkey)) {
				cent.close.rank <- nodestats$data$cent.close.rank
				val <- prettyNum(cent.close.rank, big.mark=',')
			} else {
				val <- ''
			}
			valueBox(val, "Closeness/hopness rank", color='blue')
		})
		output$nodestats.cent.close.weight <- renderValueBox({
			if (!is.null(nodestats$data$pubkey)) {
				cent.close.weight.rank <- nodestats$data$cent.close.weight.rank
				val <- prettyNum(cent.close.weight.rank, big.mark=',')
			} else {
				val <- ''
			}
			valueBox(val, "Capacity-weighted closeness/hopness rank", color='blue')
		})
		output$nodestats.nd <- renderValueBox({
			if (!is.null(nodestats$data$pubkey)) {
				nd.rank <- nodestats$data$nd.rank
				val <- prettyNum(nd.rank, big.mark=',')
			} else {
				val <- ''
			}
			valueBox(val, "Terminal rank", color='blue')
		})
		output$nodestats.bos <- renderValueBox({
			if (!is.null(nodestats$data$pubkey)) {
				bos.rank <- nodestats$data$bos.rank
				val <- prettyNum(bos.rank, big.mark=',')
			} else {
				val <- ''
			}
			valueBox(val, "BOS rank", color='blue')
		})
		output$ambossnodestats <- renderUI({
			if (!is.null(nodestats$data)){
				link <- paste0("https://amboss.space/node/", nodestats$data %>% pull(pubkey))
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
		# peer network rendering
		nodestats <- reactiveValues(data=NULL)
		observeEvent(input$nodestats_subject, {
			req(input$nodestats_subject)
			subject <- fetch_pubkey(input$nodestats_subject)
			nodestats$data <- pool %>% tbl('nodes_current') %>% filter(pubkey==subject) %>% as_tibble
			subject_nd <- pool %>% tbl('nd') %>% arrange(desc(time)) %>% filter(pubkey==subject) %>% head(1) %>% dplyr::select(pubkey, score, rank) %>% rename(c('nd.score'='score', 'nd.rank'='rank')) %>% as_tibble
			subject_bos <- pool %>% tbl('bos') %>% arrange(desc(time)) %>% filter(pubkey==subject) %>% head(1) %>% rename(c('bos.score'='score')) %>% dplyr::select(-time) %>% as_tibble
			nodestats$data <- left_join(nodestats$data, subject_nd, by='pubkey') %>% left_join(., subject_bos, by='pubkey')
			peers <- adjacent_vertices(undir_graph, subject, mode='all') %>% unlist %>% names %>% strsplit("\\.") %>% lapply(function(x) x[2]) %>% unlist
			nodestats$peers <- pool %>% tbl("nodes_historical") %>% filter(pubkey %in% peers) %>% mutate(tot.capacity=tot.capacity/1e8)
			nodestats$entries <- nodestats$peers %>% pull(time) %>% unique %>% length
			nodestats$lnsummary <- pool %>% tbl("nodes_historical") %>% filter(mean.rate.ppm<20e3) %>% group_by(time) %>% summarise(avg.node.cap=mean(tot.capacity, na.rm=TRUE), avg.rate.ppm=mean(mean.rate.ppm, na.rm=TRUE), avg.chan.size=mean(avg.capacity, na.rm=TRUE))
			nodestats$historical <- pool %>% tbl("nodes_historical") %>% filter(pubkey==local(nodestats$data$pubkey))
			nodesummary <- nodestats$historical %>% dplyr::select(time, tot.capacity, mean.rate.ppm, avg.capacity)
			nodestats$lnsummary <- left_join(nodestats$lnsummary, nodesummary) %>% as_tibble %>% drop_na
		})
		output$node_cap_change <- renderPlotly({
			nodestats$lnsummary %>%
				plot_ly(x=~time, y=~avg.node.cap, type='scatter', mode='lines', name='LN average') %>%
					add_trace(y=~tot.capacity, type='scatter', mode='lines', name=nodestats$data$alias) %>%
					layout(hovermode='x', xaxis=list(title="Date", tickangle=45), yaxis=list(title="Capacity (sat)"))
		})
		output$node_fee_change <- renderPlotly({
			nodestats$lnsummary %>%
				plot_ly(x=~time, y=~avg.rate.ppm, type='scatter', mode='lines', name='LN average') %>%
					add_trace(y=~mean.rate.ppm, type='scatter', mode='lines', name=nodestats$data$alias) %>%
					layout(hovermode='x', xaxis=list(title="Date", tickangle=45), yaxis=list(title="Average fee rate (ppm)"))
		})
		output$node_chansize_change <- renderPlotly({
			nodestats$lnsummary %>%
				plot_ly(x=~time, y=~avg.chan.size, type='scatter', mode='lines', name='LN average') %>%
					add_trace(y=~avg.capacity, type='scatter', mode='lines', name=nodestats$data$alias) %>%
					layout(hovermode='x', xaxis=list(title="Date", tickangle=45), yaxis=list(title="Average channel size (sat)"))
		})
		output$peer_overlap <- renderPlotly({
			common_peers <- fetch_peer_overlaps(nodestats$data %>% pull(pubkey))
			plot_ly(common_peers, x=~alias, y=~num_common_peers, size=~num.channels, type='scatter', mode='markers') %>%
				layout(
					xaxis=list(title="Peer alias", tickangle=45, categoryorder="array", categoryarray=common_peers$alias),
					yaxis=list(title="Number of peers in common")
				)
		})
		output$peer_ranks <- renderPlotly({
			pool %>%
				tbl('nd') %>%
				filter(time==max(time)) %>%
				filter(pubkey %in% local(nodestats$peers %>% pull(pubkey))) %>%
				as_tibble %>%
				left_join(., undir_graph %>% dplyr::select(name, alias) %>% as_tibble, by=c('pubkey'='name')) %>%
				mutate(state=ifelse(
					state=="healthy", "Healthy", ifelse(
					state=="stable", "Stable", ifelse(
					state=="fail_min_median_capacity", "Median channel capacity less than 500k sat", ifelse(
					state=="fail_max_disable_ratio", "More than 25% of channels are disabled", ifelse(
					state=="fail_uptime_ratio", "Uptime less than 99.9%", ifelse(
					state=="fail_min_chan_count", "Node has less than 10 channels", state))))))) %>%
				plot_ly(
					x=~rank, y=~good_peers, color=~state,
					type='scatter', mode='markers', size=20, alpha=0.9, text=~alias, hovertemplate=paste0("%{text}\n", "Rank: %{x}\n", "Number of good peers: %{y}")) %>%
					layout(
						xaxis=list(title="Terminal Web rank", type='log'),
						yaxis=list(title="Number of good peers"),
						legend=list(orientation='h', y=-0.15))
		})
		output$node_vs_peer_fees <- renderPlotly({
			fees <- fetch_peer_fees(undir_graph, nodestats$data %>%
				pull(pubkey)) %>%
				mutate(diff=ifelse(subject_fee>peer_fee, "Outbound fee is more than the inbound fee", ifelse(subject_fee==peer_fee, "Outbound is the same as inbound", "Outbound fee is less than the inbound fee"))) %>%
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
					legend=list(orientation='h', y=-0.15))
		})
		output$lncompare <- renderUI({
			txt <- ifelse(!is.null(nodestats$data), paste(nodestats$data %>% pull(alias), "vs the Network"), "Node vs the Network")
		})
		output$peercompare <- renderUI({
			txt <- ifelse(!is.null(nodestats$data), paste(nodestats$data %>% pull(alias), "peers"), "Node peers")
		})
		histranks_inv <- reactiveValues(invoice=NULL, cancel=FALSE, status=NULL, settled=FALSE)
		observeEvent(input$show_hist_ranks, {
			disable('show_hist_ranks')
			histranks_inv$cancel <- FALSE
			histranks_inv$settled <- FALSE
			histranks_inv$invoice <- content(POST(url=store_url, body=histranks_inv_body, config=store_headers))
			histranks_inv$status <- "Unpaid"
			showModal(
				modalDialog(
					plotOutput("histranks_qr", height='400px', width='400px'),
					title=paste("Done! Please pay", as.numeric(histranks_msat)/1e3, "sats to view results."),
					size='s',
					footer=tagList(
						rclipButton("clipbtn", "Copy", histranks_inv$invoice$BOLT11, icon("clipboard"), modal=TRUE),
						modalActionButton("histranks_cancel", "Cancel")
					)
				)
			)
			enable('show_hist_ranks')
		})
		observeEvent(input$nodestats_subject, {
			histranks_inv$settled <- FALSE
		})
		observeEvent(histranks_inv$status, {
			if (histranks_inv$cancel == TRUE ) {
				histranks_inv$status <- NULL
				removeModal()
			}
			else if (histranks_inv$status == "Unpaid") {
				delay(2000, histranks_inv$status <- 'Try again')
			}
			else if (histranks_inv$status == "Try again") {
				delay(2000, histranks_inv$status <- content(GET(url=paste0(store_url, '/', histranks_inv$invoice$id), config=store_headers))$status)
			}
			else if (histranks_inv$status == "Paid") {
				histranks_inv$settled <- TRUE
				removeModal()
			}
			else if (histranks_inv$status == "Expired") {
				removeModal()
			}
		})
		observeEvent(input$nodestats_subject, {
			histranks_inv$cancel <- TRUE
		})
		output$rank_change <- renderPlotly({
			if (histranks_inv$settled == TRUE) {
				bos <- pool %>% tbl('bos') %>% filter(time==max(time)) %>% filter(pubkey==local(nodestats$data %>% pull(pubkey))) %>% as_tibble
				nd <- pool %>% tbl('nd') %>% filter(time==max(time), pubkey==local(nodestats$data %>% pull(pubkey))) %>% as_tibble
				plt <- nodestats$historical %>%
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
			}
		})
		output$chancap_change <- renderPlotly({
			nodestats$peers %>%
				as_tibble %>%
				arrange(time, pubkey) %>%
				plot_ly(x=~num.channels, y=~tot.capacity, frame=~time, type='scatter', mode='text', text=~alias, showlegend=FALSE, hovertemplate=paste0("%{text}\n", "Capacity: %{y}\n", "Channels: %{x}")) %>%
					layout(xaxis=list(title="Number of channels", type='log'), yaxis=list(title="Total capacity (BTC)", type='log'))
		})
		output$net <- renderForceNetwork({
			req(input$nodestats_subject)
			reduce_graph <- peer_graph(undir_graph, nodestats$data %>% pull(pubkey))
			node <- reduce_graph %>%
				mutate(id=row_number()-1) %>%
				as_tibble %>%
				mutate(group=ifelse(name==nodestats$data$pubkey, 1, 2), nodesize=tot.capacity/1e8)
			links <- reduce_graph %>%
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
			fn$x$nodes$pubkey <- node$name
			fn$x$nodes$amboss <- paste0("https://amboss.space/node/", node$name)
			onRender(fn, customjs)
		})
		# summarise peer info in a table
		output$nodetable <- renderDataTable({
			req(nodestats$subject)
			req(input$show_columns)
			subject <- fetch_pubkey(nodestats$subject)
			peer_table <- peer_graph(undir_graph, subject) %>%
				mutate(id=row_number()-1) %>%
				as_tibble %>%
				mutate(group=ifelse(name==subject, 1, 2)) %>%
				dplyr::select(-c(ipv4:onion_v2), -c(amboss, oneml), -mean.delta, -c(id:cent.close)) %>%
				mutate(tot.capacity=round(tot.capacity/1e8, 2),
					avg.capacity=round(avg.capacity/1e8, 3),
					age=round(age, 0),
					mean.base.msat=round(mean.base.msat, 0),
					mean.rate.ppm=round(mean.rate.ppm, 0),
					median.base.msat=round(median.base.msat, 0),
					median.rate.ppm=round(median.rate.ppm, 0))
			peer_table_reduc <- peer_table[input$show_columns]
			col_names <- names(table_vars[which(table_vars %in% input$show_columns)])
			datatable(peer_table_reduc, colnames=col_names, options=list(pageLength=50))
		})

		output$table_vars <- renderUI({
			req(nodestats$subject)
			req(input$peerinfotab == "tablenet")
			box(width=NULL,
				checkboxGroupInput("show_columns", "Select columns to show", choices=table_vars, inline=TRUE,
					selected=table_vars[c(
						'Alias', 'Total capacity', 'Number of channels', 'Average channel capacity', 'Median base fee (msat)',
						'Median fee rate (ppm)', 'BOS score', 'Terminal Web score')]))
		})
		output$histranks_qr <- renderPlot({
			bolt11 <- histranks_inv$invoice$BOLT11
			ggqrcode(bolt11)
		})

		updateSelectizeInput(session, "nodestats_subject", choices=c("Pubkey or alias"=NULL, node_ids), selected=character(0), server=TRUE)
	})
}

nodeStatsApp <- function() {
  
	ui <- dashboardPage(
		dashboardHeader(title='Rebalance Simulator'),
		dashboardSidebar(),
		dashboardBody(nodeStatsUI('x')),
		skin='yellow',
	)
	server <- function(input, output, session) {
		nodeStatsServer('x', reactive_show=reactive(FALSE))
	}
	shinyApp(ui, server)
  
}
