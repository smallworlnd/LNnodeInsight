customjs <- read_file("inst/custom.js")

modalActionButton <- function(inputId, label, icon = NULL, width = NULL, ...) {
	value <- restoreInput(id = inputId, default = NULL)
	tags$button(id = inputId, type = "button", style = if (!is.null(width)) 
	paste0("width: ", validateCssUnit(width), ";"), type = "button", 
	class = "btn btn-default action-button", `data-dismiss` = "modal", `data-val` = value, 
	list(shiny:::validateIcon(icon), label), ...)
}

server <- function(input, output, session) {
	observeEvent(session$clientData$url_search, {
		query <- parseQueryString(session$clientData$url_search)
		if (!is.null(query[['peer_network']])) {
			pubkey <- query[['peer_network']]
			updateTabItems(session, "sidebar", "peernet")
			peernet_subject(pubkey)
			updateSelectizeInput(session, "peernet_subject", choices=c("Pubkey or alias"="", node_ids), selected=peernet_subject(), server=TRUE)
		}
	})
	# dashboard rendering
	output$chartlink <- renderInfoBox({
		infoBox(a('Build your own chart', onclick="openTab('chart')", href="#"), subtitle='Explore network-wide node data and gather insight on trends and correlations', icon=icon('chart-bar'), color='yellow')
	})
	output$peernetlink <- renderInfoBox({
		infoBox(a('Peer network', onclick="openTab('peernet')", href="#"), subtitle="Explore your node's local network and gain insight on peers", icon=icon('project-diagram', lib='font-awesome'), color='yellow')
	})
	output$rebalsimlink <- renderInfoBox({
		infoBox(a('Payment/rebalance simulator', onclick="openTab('rebalsim')", href="#"), subtitle="Estimate the potential cost of a payment or rebalance to gain insight on liquidity demand and efficient channel management", icon=icon('calculator', lib='font-awesome'), color='yellow')
	})
	output$chansimlink <- renderInfoBox({
		infoBox(a('Channel simulator', onclick="openTab('chansim')", href="#"), subtitle='Simulate opening or closing a channel on your node to measure influence in the network', icon=icon('edit'), color='yellow')
	})
	# node statistics/graph rendering
	# 
	# peer network rendering
	peernet_subject <- reactiveVal()
	observeEvent(input$peernet_subject, {
		peernet_subject(input$peernet_subject)
	})
	output$net <- renderForceNetwork({
		req(peernet_subject())
		subject <- fetch_pubkey(peernet_subject())
		reduced_g <- peer_graph(g, subject)
		node <- reduced_g %>%
			mutate(id=row_number()-1) %>%
			as_tibble %>%
			mutate(group=ifelse(name==subject, 1, 2), nodesize=tot.capacity/1e8)
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
		fn$x$nodes$amboss <- node$amboss
		fn$x$nodes$oneml <- node$oneml
		onRender(fn, customjs)
	})
	# summarise peer info in a table
	output$nodetable <- renderDataTable({
		req(peernet_subject())
		req(input$show_columns)
		subject <- fetch_pubkey(peernet_subject())
		peer_table <- peer_graph(g, subject) %>%
			mutate(id=row_number()-1) %>%
			as_tibble %>%
			mutate(group=ifelse(name==subject, 1, 2)) %>%
			select(-c(ipv4:onion_v2), -c(amboss, oneml), -mean.delta, -c(id:cent.close)) %>%
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
		req(peernet_subject())
		req(input$peerinfotab == "tablenet")
		box(width=NULL,
			checkboxGroupInput("show_columns", "Select columns to show", choices=table_vars, inline=TRUE,
				selected=table_vars[c(
					'Alias', 'Total capacity', 'Number of channels', 'Average channel capacity', 'Median base fee (msat)',
					'Median fee rate (ppm)', 'BOS score', 'Terminal Web score')]))
	})

	filtered_node <- reactiveValues()
	observeEvent(c(input$tot.capacity.filt, input$avg.capacity.filt, input$num.channels.filt, input$fee.rate.filt, input$age.filt, input$cent.between.rank.filt, input$cent.close.rank.filt, input$cent.eigen.rank.filt, input$community.filt, input$pubkey.or.alias), {
		if (is.null(input$community.filt)) {
			community.filt <- g %>% as_tibble %>% select(community) %>% unique %>% pull
		} else {
			community.filt <- input$community.filt
		}
		filt.aliases <- g %>%
			as_tibble %>%
			filter(
				!is.na(alias),
				tot.capacity>=input$tot.capacity.filt[1]*1e8, tot.capacity<=input$tot.capacity.filt[2]*1e8,
				avg.capacity>=input$avg.capacity.filt[1]*1e8, avg.capacity<=input$avg.capacity.filt[2]*1e8,
				num.channels>=input$num.channels.filt[1], num.channels<=input$num.channels.filt[2],
				median.rate.ppm>=input$fee.rate.filt[1], median.rate.ppm<=input$fee.rate.filt[2],
				age>=input$age.filt[1], age<=input$age.filt[2],
				cent.between.rank>=input$cent.between.rank.filt[1], cent.between.rank<=input$cent.between.rank.filt[2],
				cent.close.rank>=input$cent.close.rank.filt[1], cent.close.rank<=input$cent.close.rank.filt[2],
				cent.eigen.rank>=input$cent.eigen.rank.filt[1], cent.eigen.rank<=input$cent.eigen.rank.filt[2]) %>%
			select(alias) %>%
			pull
		filt.pubkeys <- g %>%
			as_tibble %>%
			filter(
				tot.capacity>=input$tot.capacity.filt[1]*1e8, tot.capacity<=input$tot.capacity.filt[2]*1e8,
				avg.capacity>=input$avg.capacity.filt[1]*1e8, avg.capacity<=input$avg.capacity.filt[2]*1e8,
				num.channels>=input$num.channels.filt[1], num.channels<=input$num.channels.filt[2],
				median.rate.ppm>=input$fee.rate.filt[1], median.rate.ppm<=input$fee.rate.filt[2],
				age>=input$age.filt[1], age<=input$age.filt[2],
				cent.between.rank>=input$cent.between.rank.filt[1], cent.between.rank<=input$cent.between.rank.filt[2],
				cent.close.rank>=input$cent.close.rank.filt[1], cent.close.rank<=input$cent.close.rank.filt[2],
				cent.eigen.rank>=input$cent.eigen.rank.filt[1], cent.eigen.rank<=input$cent.eigen.rank.filt[2]) %>%
			select(name) %>%
			pull
		if (input$pubkey.or.alias == 3) {
			filtered_node$list <- c(filt.aliases, filt.pubkeys)
		} else if (input$pubkey.or.alias == 1) {
			filtered_node$list <- filt.pubkeys
		} else {
			filtered_node$list <- filt.aliases
		}
		updateSelectizeInput(session, "target", choices=c("Pubkey or alias"="", filtered_node$list), selected=character(0), server=TRUE)
		updateSelectizeInput(session, "target2", choices=c("Pubkey or alias"="", filtered_node$list), selected=character(0), server=TRUE)
		updateSelectizeInput(session, "target3", choices=c("Pubkey or alias"="", filtered_node$list), selected=character(0), server=TRUE)
	})
	# channel simulation
	output$ambosslink<- renderUI({
		if (input$chansim_subject != ""){
			link <- paste0("https://amboss.space/node/", fetch_pubkey(input$chansim_subject))
		} else {
			link <- "https://amboss.space"
		}

		tags$a(
			href=link, 
			tags$img(src="www/AmbossLogo.png", 
				width="100%",
				height="100%"),
			target="_blank")

	})
	updateSelectizeInput(session, "peernet_subject", choices=c("Pubkey or alias"=NULL, node_ids), selected=character(0), server=TRUE)
	updateSelectizeInput(session, "chansim_subject", choices=c("Pubkey or alias"=NULL, node_ids), selected=character(0), server=TRUE)
	updateSelectizeInput(session, "rebalsim_subject", choices=c("Pubkey or alias"=NULL, node_ids), selected=character(0), server=TRUE)
	updateSelectizeInput(session, "rebalsim_out_node", choices=c("Pubkey or alias"=NULL, node_ids), selected=character(0), server=TRUE)
	updateSelectizeInput(session, "rebalsim_in_node", choices=c("Pubkey or alias"=NULL, node_ids), selected=character(0), server=TRUE)
	updateSelectizeInput(session, "paysim_in_node", choices=c("Pubkey or alias"=NULL, node_ids), selected=character(0), server=TRUE)
	updateSelectizeInput(session, "paysim_out_node", choices=c("Pubkey or alias"=NULL, node_ids), selected=character(0), server=TRUE)

	rebal_inv <- reactiveValues(invoice=NULL, cancel=FALSE, status=NULL, settled=FALSE)
	payrebal_sim <- reactiveValues()
	observeEvent(c(input$launch_payrebalsim, input$pay_or_rebal), {
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
		rebal_inv$invoice <- content(POST(url=base, body=rebalsim_inv_body, config=headers))
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
			delay(2000, rebal_inv$status <- content(GET(url=paste0(base, '/', rebal_inv$invoice$id), config=headers))$status)
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
	output$rebal_flowcost_scatter <- renderPlotly({
		if (rebal_inv$settled == TRUE) {
			plot_ly(payrebal_sim$values %>% as_tibble,
				x=~max_path_flow, y=~path_fee, showlegend=TRUE,
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
		valueBox(val, desc, color="blue")
	})

	output$maxflowinfo <- renderInfoBox({
		infoBox(title=NULL, subtitle='Maximum flow is the highest amount of sats that can theoretically be pushed through a path if liquidity were 100% outbound. In reality, outbound across a path is likely 50% or less.', icon=icon('question', lib='font-awesome'), color='yellow')
	})
	output$rebal_qr <- renderPlot({
		bolt11 <- rebal_inv$invoice$BOLT11
		ggqrcode(bolt11)
	})

	chan_sim_parms <- reactiveValues()
	status <- reactiveVal()
	observeEvent(input$launch_sim, {
		req(input$chansim_subject)
		req(input$target != "" || input$target2 != "" || input$target3 != "")
		status('latest')
		subject <- fetch_pubkey(input$chansim_subject)
		target <- sapply(
			c(input$target, input$target2, input$target3),
			function(x) fetch_pubkey(x))
		target <- na.omit(target) %>% as.vector
		indels <- c(input$add_or_del, input$add_or_del2, input$add_or_del3)
		indels <- indels[1:length(target)]
		showModal(modalDialog("Running simulation, please wait...", size='s', footer=NULL))
		sim_graph <- sim_chan(subject, target, indels)
		removeModal()
		chan_sim_parms$subject <- subject
		chan_sim_parms$graph <- sim_graph$graph
		chan_sim_parms$betw <- sim_graph$betw
		chan_sim_parms$clo <- sim_graph$clo
		chan_sim_parms$eigen <- sim_graph$eigen
	})
	observeEvent({list(input$chansim_subject, input$target, input$target2, input$target3)}, {
		status('changed')
	})
	# build value boxes with summary stats for the specified node
	output$cent.between <- renderValueBox({
		if (status() == "latest") {
			cent.between.rank <- chan_sim_parms$graph %>%
				filter(name==chan_sim_parms$subject) %>%
				select(sim.cent.between.rank) %>%
				pull %>%
				as.vector
			delta <- chan_sim_parms$graph %>%
				filter(name==chan_sim_parms$subject) %>%
				select(cent.between.rank.delta) %>%
				pull %>% as.vector
			qualifier <- ifelse(delta>0, 'gain', ifelse(delta==0, 'no change', 'lose'))
			color <- ifelse(delta>0, "green", ifelse(delta==0, "blue", "red"))
			val <- ifelse(delta==0,
				paste0(prettyNum(cent.between.rank, big.mark=','), ' (', qualifier, ')'),
				paste0(prettyNum(cent.between.rank, big.mark=','), " (", qualifier, " ", abs(delta), ")"))
		} else if (input$chansim_subject != '') {
			subject <- fetch_pubkey(input$chansim_subject)
			color <- 'blue'
			val <- prettyNum(g %>% as_tibble %>% filter(name==subject) %>% select(cent.between.rank) %>% pull, big.mark=',')
		} else {
			val <- ''
			color <- 'blue'
		}
		valueBox(val, "Betweenness centrality rank", color=color)
	})
	output$cent.eigen <- renderValueBox({
		if (status() == "latest") {
			cent.eigen.rank <- chan_sim_parms$graph %>%
				filter(name==chan_sim_parms$subject) %>%
				select(sim.cent.eigen.rank) %>%
				pull %>% as.vector
			delta <- chan_sim_parms$graph %>%
				filter(name==chan_sim_parms$subject) %>%
				select(cent.eigen.rank.delta) %>%
				pull %>% as.vector
			qualifier <- ifelse(delta>0, 'gain', ifelse(delta==0, 'no change', 'lose'))
			color <- ifelse(delta>0, "green", ifelse(delta==0, "blue", "red"))
			val <- ifelse(delta==0,
				paste0(prettyNum(cent.eigen.rank, big.mark=','), ' (', qualifier, ')'),
				paste0(prettyNum(cent.eigen.rank, big.mark=','), " (", qualifier, " ", abs(delta), ")"))
		} else if (input$chansim_subject != '') {
			subject <- fetch_pubkey(input$chansim_subject)
			color <- 'blue'
			val <- prettyNum(g %>% as_tibble %>% filter(name==subject) %>% select(cent.eigen.rank) %>% pull, big.mark=',')
		} else {
			val <- ''
			color <- 'blue'
		}
		valueBox(val, "Eigenvector/hubness centrality rank", color=color)
	})
	output$cent.close <- renderValueBox({
		if (status() == "latest") {
			cent.close.rank <- chan_sim_parms$graph %>%
				filter(name==chan_sim_parms$subject) %>%
				select(sim.cent.close.rank) %>%
				pull %>% as.vector
			delta <- chan_sim_parms$graph %>%
				filter(name==chan_sim_parms$subject) %>%
				select(cent.close.rank.delta) %>%
				pull %>% as.vector
			qualifier <- ifelse(delta>0, 'gain', ifelse(delta==0, 'no change', 'lose'))
			color <- ifelse(delta>0, "green", ifelse(delta==0, "blue", "red"))
			val <- ifelse(delta==0,
				paste0(prettyNum(cent.close.rank, big.mark=','), ' (', qualifier, ')'),
				paste0(prettyNum(cent.close.rank, big.mark=','), " (", qualifier, " ", abs(delta), ")"))
		} else if (input$chansim_subject != '') {
			subject <- fetch_pubkey(input$chansim_subject)
			color <- 'blue'
			val <- prettyNum(g %>% as_tibble %>% filter(name==subject) %>% select(cent.close.rank) %>% pull, big.mark=',')
		} else {
			val <- ''
			color <- 'blue'
		}
		valueBox(val, "Closeness/hopness centrality rank", color=color)
	})
	# peer overlap for selected nodes
	output$chansim_venn <- renderPlotly({
		req(input$chansim_subject)
		req(input$target != "" || input$target2 != "" || input$target3 != "")
		subject <- fetch_pubkey(input$chansim_subject)
		subject_alias <- fetch_alias(subject)
		target <- sapply(
			c(input$target, input$target2, input$target3),
			function(x) fetch_pubkey(x))
		target <- na.omit(target) %>% as.vector
		target_aliases <- sapply(target, function(x) fetch_alias(x))
		peers <- lapply(c(subject, target), function(x) fetch_peer_aliases(x))
		names(peers) <- c(subject_alias, target_aliases)
		#ggVennDiagram(peers, show_intersect=TRUE)
		venn <- Venn(peers)
		data <- process_data(venn)
		items <- venn_region(data) %>%
			dplyr::rowwise() %>%
			dplyr::mutate(text = stringr::str_wrap(paste0(.data$item, collapse = " "), width = 40)) %>%
			sf::st_as_sf()
		label_coord = sf::st_centroid(items$geometry) %>% sf::st_coordinates()
		p <- ggplot(items) +
			geom_sf(aes_string(fill="count")) +
			geom_sf_text(aes_string(label = "name"),
				data = data@setLabel,
				inherit.aes = F) +
			geom_text(aes_string(label = "count", text = "text"),
				x = label_coord[,1],
				y = label_coord[,2],
				show.legend = FALSE) +
			theme_void() +
			scale_fill_distiller(palette = "RdYlBu")
		ax <- list(showline = FALSE)
		plotly::ggplotly(p, tooltip = c("text")) %>%
			plotly::layout(xaxis = ax, yaxis = ax)
	})

	# network-wide centralizations
	output$between.centralization <- renderValueBox({
		if (status() == "latest") {
			delta <- (chan_sim_parms$betw$centralization - g_betw$centralization) / g_betw$centralization * 100
			qualifier <- ifelse(delta>=0, '+', '-')
			val <- paste0(signif(chan_sim_parms$betw$centralization, 4), " (", qualifier, " ", abs(signif(delta, 2)), "%)")
		} else {
			val <- signif(g_betw$centralization, 4)
		}
		valueBox(val, "Betweenness centralization", color='blue')
	})
	# network-wide centralizations
	output$closeness.centralization <- renderValueBox({
		if (status() == "latest") {
			delta <- (chan_sim_parms$clo$centralization - g_clo$centralization) / g_clo$centralization * 100
			qualifier <- ifelse(delta>=0, '+', '-')
			val <- paste0(signif(chan_sim_parms$clo$centralization, 4), " (", qualifier, " ", abs(signif(delta, 2)), "%)")
		} else {
			val <- signif(g_clo$centralization, 4)
		}
		valueBox(val, "Closeness centralization", color='blue')
	})
	# network-wide centralizations
	output$eigen.centralization <- renderValueBox({
		if (status() == "latest") {
			delta <- (chan_sim_parms$eigen$centralization - g_eigen$centralization) / g_eigen$centralization * 100
			qualifier <- ifelse(delta>=0, '+', '-')
			val <- paste0(signif(chan_sim_parms$eigen$centralization, 4), " (", qualifier, " ", abs(signif(delta, 2)), "%)")
		} else {
			val <- signif(g_eigen$centralization, 4)
		}
		valueBox(val, "Eigenvector centralization", color='blue')
	})
	# scatterplotting
	output$scatter <- renderPlotly({
		req(input$scatter_x, input$scatter_y)
		plot_ly(g %>% as_tibble,
			x=as.formula(paste0('~', chart_vars[input$scatter_x])),
			y=as.formula(paste0('~', chart_vars[input$scatter_y])),
			height='725') %>%
				layout(
					xaxis=list(title=input$scatter_x, type='log'),
					yaxis=list(title=input$scatter_y, type='log'))
	})
	observeEvent(input$scatterclear, {
		updateSelectizeInput(session, inputId='scatter_x', label='Choose an X-axis variable', choices=c('', names(chart_vars)))
		updateSelectizeInput(session, inputId='scatter_y', label='Choose a Y-axis variable', choices=c('', names(chart_vars)))
	})
	# histograms
	output$histo <- renderPlotly({
		req(input$histo_x)
		plot_ly(g %>% as_tibble,
			x=as.formula(paste0('~', chart_vars[input$histo_x])),
			height='725') %>%
				layout(
					xaxis=list(title=input$histo_x,type='linear'),
					yaxis=list(title="Number of nodes with", type='log'))
	})
	observeEvent(input$histoclear, {
		updateSelectizeInput(session, inputId='histo_x', label='Choose a variable', choices=c('', names(chart_vars)))
	})
}
