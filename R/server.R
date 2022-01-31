customjs <- read_file("inst/custom.js")

node_ids <- paste(g %>% pull(alias), "-", g %>% pull(name))

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

modalActionButton <- function(inputId, label, icon = NULL, width = NULL, ...) {
	value <- restoreInput(id = inputId, default = NULL)
	tags$button(id = inputId, type = "button", style = if (!is.null(width)) 
	paste0("width: ", validateCssUnit(width), ";"), type = "button", 
	class = "btn btn-default action-button", `data-dismiss` = "modal", `data-val` = value, 
	list(shiny:::validateIcon(icon), label), ...)
}

server <- function(input, output, session) {
	credentials <- loginServer(
		id="login",
		data=users,
		pubkey_col='pubkey',
		cookie_logins=TRUE,
		reload_on_logout=TRUE,
		sessionid_col='sessionid',
		cookie_getter=get_sessions_from_db,
		cookie_setter=add_session_to_db,
		rest_url_base=rest_url,
		rest_headers=rest_headers,
		rest_content=rest_content,
		log_out=reactive(logout_init())
	)
	logout_init <- logoutServer(
		id="logout",
		active=reactive(credentials()$user_auth)
	)
	output$login_nav <- renderUI({
		req(!credentials()$user_auth)
		actionBttn(inputId='login_nav', label='Login', style='fill', color='success', block=FALSE, size='sm')
	})
	onclick('login_nav', updateTabItems(session, "sidebar", "account"))
	output$account_page <- renderUI({
		req(credentials()$user_auth)
		acc <- users %>% filter(pubkey==!!pull(credentials()$info[1])) %>% as_tibble
		column(8, offset=2,
			box(title=NULL, background='yellow', width=12,
				p(style="text-align: left; font-size: 20px", strong(paste(acc$alias, "account page"))),
				hr(),
				fluidRow(
					column(6, align='left',
						p('Subscription')
					),
					column(6, align='right',
						p(acc$permissions)
					),
				)
			)
		)
	})

    runjs("
      $('.box').on('click', '.box-header h3', function() {
          $(this).closest('.box')
                 .find('[data-widget=collapse]')
                 .click();
      });")
	removeCssClass("ss-overlay", "ss-gray-out")
	observeEvent(input$sidebar, {
		req(input$sidebar == "support")
		showModal(
			modalDialog(
				title="Thanks for considering supporting us!",
				h4("Our Lightning Address:"),
				h2(a("smallworlnd@btcpay.lnnodeinsight.com")),
				size='l',
				easyClose=TRUE,
				footer=tagList(
					modalActionButton("donate_cancel", "Cancel")
				)
			)
		)
	})
	observeEvent(session$clientData$url_search, {
		query <- parseQueryString(session$clientData$url_search)
		if (length(query)>0) {
			pubkey <- str_split(names(query), '/')[[1]][2]
			pubkey_search <- node_ids[grepl(pubkey, node_ids)]
			if (length(pubkey_search) > 0) {
				updateTabItems(session, "sidebar", "nodestats")
				updateSelectizeInput(session, "nodestats_subject", choices=c("Pubkey/alias"="", node_ids), selected=pubkey_search, server=TRUE)
			}
		}
	})
	# dashboard rendering
	output$lnrouterlink <- renderInfoBox({
		ic <- apputils::icon(list(src="www/lnrouter.png", width="90px"), lib="local")
		apputils::infoBox(tags$a("lnrouter", href="https://lnrouter.app", target="_blank"), subtitle='Explore channel balancedness with LnRouter to help identify well-managed nodes', icon=ic, color=NULL)
	})
	output$lnrouterlink2 <- renderInfoBox({
		ic <- apputils::icon(list(src="www/lnrouter.png", width="90px"), lib="local")
		apputils::infoBox(tags$a("LnRouter", href="https://lnrouter.app", target="_blank"), subtitle='Combine the results of the rebalance simulator with LookUps to get a full picture of the liquidity availability of a node and its neighbourhood', icon=ic, color=NULL)
	})
	output$ambosslink <- renderInfoBox({
		ic <- apputils::icon(list(src="www/amboss.png", width="90px"), lib="local")
		apputils::infoBox(a("amboss", href="https://amboss.space", target="_blank"), subtitle='Explore a node\'s channels, fees and all the most useful summary stats at Amboss', icon=ic, color='NULL')
	})
	output$chartlink <- renderInfoBox({
		infoBox(a('Build your own chart', onclick="openTab('chart')", href="#"), subtitle='Explore network-wide node data and gather insight on trends and correlations', icon=icon('chart-bar'), color='yellow')
	})
	output$nodestatslink <- renderInfoBox({
		infoBox(a('Node stats', onclick="openTab('nodestats')", href="#"), subtitle="Explore your node's local network and gain insight on peers", icon=icon('project-diagram', lib='font-awesome'), color='yellow')
	})
	output$rebalsimlink <- renderInfoBox({
		infoBox(a('Payment/rebalance simulator', onclick="openTab('rebalsim')", href="#"), subtitle="Estimate the potential cost of a payment or rebalance to gain insight on liquidity demand and bottlenecks", icon=icon('calculator', lib='font-awesome'), color='yellow')
	})
	output$chansimlink <- renderInfoBox({
		infoBox(a('Channel simulator', onclick="openTab('chansim')", href="#"), subtitle='Simulate opening or closing a channel on your node to measure influence in the network', icon=icon('edit'), color='yellow')
	})
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
		nodestats$data <- nodes_latest %>% filter(pubkey==subject) %>% as_tibble
		subject_nd <- nd_latest %>% filter(pubkey==subject) %>% dplyr::select(pubkey, score, rank) %>% rename(c('nd.score'='score', 'nd.rank'='rank')) %>% as_tibble
		subject_bos <- bos_latest %>% filter(pubkey==subject) %>% rename(c('bos.score'='score', 'bos.rank'='rank')) %>% dplyr::select(-time) %>% as_tibble
		nodestats$data <- left_join(nodestats$data, subject_nd, by='pubkey') %>% left_join(., subject_bos, by='pubkey')
		peers <- adjacent_vertices(g, subject, mode='all') %>% unlist %>% names %>% strsplit("\\.") %>% lapply(function(x) x[2]) %>% unlist
		nodestats$peers <- nodes_agg_3m %>% filter(pubkey %in% peers) %>% mutate(tot.capacity=tot.capacity/1e8)
		nodestats$entries <- nodestats$peers %>% pull(time) %>% unique %>% length
		nodestats$lnsummary <- nodes_agg_3m %>% filter(mean.rate.ppm<20e3) %>% group_by(time) %>% summarise(avg.node.cap=mean(tot.capacity, na.rm=TRUE), avg.rate.ppm=mean(mean.rate.ppm, na.rm=TRUE), avg.chan.size=mean(avg.capacity, na.rm=TRUE))
		nodestats$historical <- nodes_agg_3m %>% filter(pubkey==local(nodestats$data$pubkey))
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
		nd_latest %>%
			filter(pubkey %in% local(nodestats$peers %>% pull(pubkey))) %>%
			as_tibble %>%
			left_join(., g %>% dplyr::select(name, alias) %>% as_tibble, by=c('pubkey'='name')) %>%
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
		fees <- fetch_peer_fees(g, nodestats$data %>%
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
			bos <- bos_agg %>% filter(pubkey==local(nodestats$data %>% pull(pubkey)), time>=today()-months(3)) %>% as_tibble
			nd <- nd_agg %>% filter(time>=today()-months(3), pubkey==local(nodestats$data %>% pull(pubkey))) %>% as_tibble
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
		reduced_g <- peer_graph(g, nodestats$data %>% pull(pubkey))
		node <- reduced_g %>%
			mutate(id=row_number()-1) %>%
			as_tibble %>%
			mutate(group=ifelse(name==nodestats$data$pubkey, 1, 2), nodesize=tot.capacity/1e8)
		links <- reduced_g %>%
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
		peer_table <- peer_graph(g, subject) %>%
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

	# channel simulation
	updateSelectizeInput(session, "nodestats_subject", choices=c("Pubkey or alias"=NULL, node_ids), selected=character(0), server=TRUE)
	updateSelectizeInput(session, "rebalsim_subject", choices=c("Pubkey or alias"=NULL, g_dir_node_ids), selected=character(0), server=TRUE)
	updateSelectizeInput(session, "rebalsim_out_node", choices=c("Pubkey or alias"=NULL, g_dir_node_ids), selected=character(0), server=TRUE)
	updateSelectizeInput(session, "rebalsim_in_node", choices=c("Pubkey or alias"=NULL, g_dir_node_ids), selected=character(0), server=TRUE)
	updateSelectizeInput(session, "paysim_in_node", choices=c("Pubkey or alias"=NULL, g_dir_node_ids), selected=character(0), server=TRUE)
	updateSelectizeInput(session, "paysim_out_node", choices=c("Pubkey or alias"=NULL, g_dir_node_ids), selected=character(0), server=TRUE)

	rebal_inv <- reactiveValues(invoice=NULL, cancel=FALSE, status=NULL, settled=FALSE)
	payrebal_sim <- reactiveValues()
	observeEvent(c(input$launch_payrebalsim, input$pay_or_rebal), {
		req(input$launch_payrebalsim)
		disable('launch_payrebalsim')
		if (input$pay_or_rebal == 1) {
			req(input$rebalsim_subject)
			req(input$rebalsim_in_node)
			req(input$rebalsim_out_node)
			subject <- input$rebalsim_subject
			out_node <- input$rebalsim_out_node
			in_node <- input$rebalsim_in_node
		} else {
			req(input$paysim_in_node)
			req(input$paysim_out_node)
			subject <- ''
			out_node <- input$paysim_out_node
			in_node <- input$paysim_in_node
		}
		rebal_inv$cancel <- FALSE
		rebal_inv$settled <- FALSE
		showModal(
			modalDialog(
				"Running simulation, please wait...",
				size='s', footer='It should take a few seconds. An invoice will be displayed when the results are ready.'))
		payrebal_sim$values <- path_flow_cost(in_graph=g_dir, subject=subject, out_node=out_node, in_node=in_node)

#chan.bals <- chan.state %>% filter(key=='inbound' | key=='outbound') %>% mutate(from.key=ifelse(key=='inbound', value, name), to.key=ifelse(key=='outbound', value, name)) %>% dplyr::select(from.key, to.key) %>% left_join(ids, by=c('from.key'='name')) %>% left_join(ids, by=c('to.key'='name')) %>% rename(c('from'='id.x', 'to'='id.y')) %>% mutate(balance='min1M') %>% dplyr::select(from, to, balance) %>% unique
#all_edges <- left_join(all_edges, chan.bals)
#node.redflags <- chan.state %>% filter(key=='low_median_capacity' | key=='low_chan_count' | key=='many_disabled_channels' | key=='suboptimal_uptime') %>% dplyr::select(name, key) %>% rename('state'='key') %>% unique
#nodes <- left_join(nodes, node.redflags) %>% mutate(state=ifelse(is.na(last_update), "inactive", ifelse(last_update>14, "inactive", state))) %>% mutate(state=replace_na(state, "healthy")) 

#chan.bals <- nd %>% filter(key=='inbound' | key=='outbound') %>% mutate(from.key=ifelse(key=='inbound', value, name), to.key=ifelse(key=='outbound', value, name)) %>% dplyr::select(from.key, to.key) %>% left_join(ids, by=c('from.key'='name')) %>% left_join(ids, by=c('to.key'='name')) %>% rename(c('from'='id.x', 'to'='id.y')) %>% mutate(balance='min1M') %>% dplyr::select(from, to, balance) %>% unique
#all_edges <- left_join(all_edges, chan.bals)
#node.redflags <- nd %>% filter(key=='low_median_capacity' | key=='low_chan_count' | key=='many_disabled_channels' | key=='suboptimal_uptime') %>% dplyr::select(name, key) %>% rename('state'='key') %>% unique

		rebal_inv$invoice <- content(POST(url=store_url, body=rebalsim_inv_body, config=store_headers))
		rebal_inv$status <- "Unpaid"
		removeModal()
		showModal(
			modalDialog(
				plotOutput("rebal_qr", height='400px', width='400px'),
				title=paste("Done! Please pay", as.numeric(rebalsim_msat)/1e3, "sats to view results."),
				size='s',
				footer=tagList(
					rclipButton("clipbtn", "Copy", rebal_inv$invoice$BOLT11, icon("clipboard"), modal=TRUE),
					modalActionButton("rebalsim_cancel", "Cancel")
				)
			)
		)
		enable('launch_payrebalsim')
	})
	observeEvent({list(input$rebalsim_subject, input$rebalsim_in_node, input$rebalsim_out_node)}, {
		rebal_inv$settled <- FALSE
	})
	observeEvent(rebal_inv$status, {		
		if (rebal_inv$cancel == TRUE ) {
			rebal_inv$status <- NULL
			removeModal()
		}
		else if (rebal_inv$status == "Unpaid") {
			delay(2000, rebal_inv$status <- 'Try again')
		}
		else if (rebal_inv$status == "Try again") {
			delay(2000, rebal_inv$status <- content(GET(url=paste0(store_url, '/', rebal_inv$invoice$id), config=store_headers))$status)
		}
		else if (rebal_inv$status == "Paid") {
			rebal_inv$settled <- TRUE
			removeModal()
		}
		else if (rebal_inv$status == "Expired") {
			removeModal()
		}
	})
	observeEvent(input$rebalsim_cancel, {
		rebal_inv$cancel <- TRUE
	})
	rebalsim_res_histo_tab <- reactiveVal()
	observeEvent(input$rebalsim_res_histo, {
		if (input$rebalsim_res_histo == 'rebalsim_cost_histo') {
			rebalsim_res_histo_tab('cost')
		}
		else if (input$rebalsim_res_histo == 'rebalsim_flow_histo') {
			rebalsim_res_histo_tab('flow')
		}
		else if (input$rebalsim_res_histo == 'rebalsim_bal_histo') {
			rebalsim_res_histo_tab('bal')
		}
	})
	output$rebal_cost_histo <- renderPlotly({
		if (rebal_inv$settled == TRUE) {
			plot_ly(payrebal_sim$values$path_fee %>% as_tibble,
				x=~value, nbinsx=50) %>%
					layout(
						xaxis=list(title="Total path cost (ppm)"),
						yaxis=list(title="Number of paths"))
		}
	})
	output$rebal_flow_histo <- renderPlotly({
		if (rebal_inv$settled == TRUE) {
			plot_ly(payrebal_sim$values$max_path_flow %>% as_tibble,
				x=~value, nbinsx=50) %>%
					layout(
						xaxis=list(title="Path maximum liquidity flow (sat)"),
						yaxis=list(title="Number of paths"))
		}
	})
	output$rebal_bal_histo <- renderPlotly({
		if (rebal_inv$settled == TRUE) {
			plot_ly(payrebal_sim$values$known_1Mmin %>% as_tibble,
				x=~value, nbinsx=50) %>%
					layout(
						xaxis=list(title="Percentage of channels in path with at least 1M routable sats"),
						yaxis=list(title="Number of paths"))
		}
	})
	output$rebal_flowcost_scatter <- renderPlotly({
		if (rebal_inv$settled == TRUE) {
			plot_ly(payrebal_sim$values %>% as_tibble,
				x=~max_path_flow, y=~path_fee, showlegend=TRUE, type='scatter',
				marker=list(
					color=~path_hops,
					size=15,
					colorscale="RdBu",
					colorbar=list(title='# of hops'),
					opacity=0.4)) %>%
					layout(
						xaxis=list(title="Path maximum liquidity flow (sat)"),
						yaxis=list(title="Path cost (ppm)"))
		}
	})
	output$rebal_flowbal_scatter <- renderPlotly({
		if (rebal_inv$settled == TRUE) {
			plot_ly(payrebal_sim$values %>% as_tibble,
				x=~max_path_flow, y=~known_1Mmin, showlegend=TRUE, type='scatter',
				marker=list(
					color=~path_hops,
					size=15,
					colorscale="RdBu",
					colorbar=list(title='# of hops'),
					opacity=0.4)) %>%
					layout(
						xaxis=list(title="Path maximum liquidity flow (sat)"),
						yaxis=list(title="% of path with known minimum of 1M routable sats"))
		}
	})
	output$rebal_balcost_scatter <- renderPlotly({
		if (rebal_inv$settled == TRUE) {
			plot_ly(payrebal_sim$values %>% as_tibble,
				x=~path_fee, y=~known_1Mmin, showlegend=TRUE, type='scatter',
				marker=list(
					color=~path_hops,
					size=15,
					colorscale="RdBu",
					colorbar=list(title='# of hops'),
					opacity=0.4)) %>%
					layout(
						xaxis=list(title="Path cost (ppm)"),
						yaxis=list(title="% of path with known minimum of 1M routable sats"))
		}
	})
	output$rebalsim.samples <- renderValueBox({
		if (rebal_inv$settled == TRUE) {
			val <- payrebal_sim$values$path_fee %>% length
		} else {
			val <- ''
		}
		valueBox(val, "Number of paths sampled", color="blue")
	})
	output$rebalsim.min <- renderValueBox({
		if (rebalsim_res_histo_tab() == 'cost') {
			desc <- "Lowest path cost (ppm)"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$path_fee %>% min %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		else if (rebalsim_res_histo_tab() == 'flow') {
			desc <- "Lowest maximum liquidity flow (sat)"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$max_path_flow %>% min %>% as.integer %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		else if (rebalsim_res_histo_tab() == 'bal') {
			desc <- "Lowest percentage of channels with high liquidity availability"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$known_1Mmin %>% min %>% as.integer %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		valueBox(val, desc, color="blue")
	})
	output$rebalsim.max <- renderValueBox({
		if (rebalsim_res_histo_tab() == 'cost') {
			desc <- "Highest path cost (ppm)"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$path_fee %>% max %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		else if (rebalsim_res_histo_tab() == 'flow') {
			desc <- "Highest maximum liquidity flow (sat)"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$max_path_flow %>% max %>% as.integer %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		else if (rebalsim_res_histo_tab() == 'bal') {
			desc <- "Highest percentage of channels with high liquidity availability"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$known_1Mmin %>% max %>% round(0) %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		valueBox(val, desc, color="blue")
	})
	output$rebalsim.avg <- renderValueBox({
		if (rebalsim_res_histo_tab() == 'cost') {
			desc <- "Expected path cost (ppm)"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$path_fee %>% mean %>% round(0) %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		else if (rebalsim_res_histo_tab() == 'flow') {
			desc <- "Expected maximum liquidity flow (sat)"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$max_path_flow %>% mean %>% round(0) %>% as.integer %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		else if (rebalsim_res_histo_tab() == 'bal') {
			desc <- "Expected percentage of channels with high liquidity availability"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$known_1Mmin %>% mean %>% round(0) %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		valueBox(val, desc, color="blue")
	})
	output$rebalsim.med <- renderValueBox({
		if (rebalsim_res_histo_tab() == 'cost') {
			desc <- "Median path cost (ppm)"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$path_fee %>% median %>% round(0) %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		else if (rebalsim_res_histo_tab() == 'flow') {
			desc <- "Median path maximum liquidity flow (sat)"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$max_path_flow %>% median %>% round(0) %>% as.integer %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		else if (rebalsim_res_histo_tab() == 'bal') {
			desc <- "Median percentage of channels with high liquidity availability"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$known_1Mmin %>% median %>% round(0) %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		valueBox(val, desc, color="blue")
	})
	output$rebalsim.sd <- renderValueBox({
		if (rebalsim_res_histo_tab() == 'cost') {
			desc <- "Spread in path cost (ppm)"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$path_fee %>% sd %>% round(0) %>% as.integer %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		else if (rebalsim_res_histo_tab() == 'flow') {
			desc <- "Spread in path maximum liquidity flow (sat)"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$max_path_flow %>% sd %>% round(0) %>% as.integer %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		else if (rebalsim_res_histo_tab() == 'bal') {
			desc <- "Spread in percentage of channels with high liquidity availability"
			if (rebal_inv$settled == TRUE) {
				val <- payrebal_sim$values$known_1Mmin %>% sd %>% round(0) %>% prettyNum(big.mark=',')
			} else {
				val <- ''
			}
		}
		valueBox(val, desc, color="blue")
	})
	output$rebal_qr <- renderPlot({
		bolt11 <- rebal_inv$invoice$BOLT11
		ggqrcode(bolt11)
	})
	output$histranks_qr <- renderPlot({
		bolt11 <- histranks_inv$invoice$BOLT11
		ggqrcode(bolt11)
	})

	chansimServer('chansim', reactive(credentials()$user_auth))
	byocServer('byoc', reactive_show=reactive(credentials()$user_auth))
}
