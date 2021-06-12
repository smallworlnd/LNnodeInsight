customjs <- read_file("inst/custom.js")

server <- function(input, output, session) {
	# dashboard rendering
	output$chartlink <- renderInfoBox({
		infoBox(a('Build your own chart', onclick="openTab('chart')", href="#"), subtitle='Explore network-wide node data and gather insight on trends', icon=icon('chart-bar'), color='yellow')
	})
	output$peernetlink <- renderInfoBox({
		infoBox(a('Peer network', onclick="openTab('peernet')", href="#"), subtitle="Explore your node's local network and gain insight on peers", icon=icon('project-diagram', lib='font-awesome'), color='yellow')
	})
	output$chansimlink <- renderInfoBox({
		infoBox(a('Channel simulator', onclick="openTab('chansim')", href="#"), subtitle='Simulate opening or closing a channel on your node to measure influence in the network', icon=icon('edit'), color='yellow')
	})

	# peer network rendering
	view_node <- reactiveVal()
	observeEvent(input$view_node, {
		view_node(input$view_node)
	})
	output$net <- renderForceNetwork({
		req(view_node())
		subject <- fetch_pubkey(view_node())
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
		req(view_node())
		req(input$show_columns)
		subject <- fetch_pubkey(view_node())
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
		req(view_node())
		req(input$peerinfotab == "tablenet")
		box(width=NULL,
			checkboxGroupInput("show_columns", "Select columns to show", choices=table_vars, inline=TRUE,
				selected=table_vars[c(
					'Alias', 'Total capacity', 'Number of channels', 'Average channel capacity', 'Median base fee (msat)',
					'Median fee rate (ppm)', 'BOS score', 'Terminal Web score')]))
	})

	# channel simulation
	updateSelectizeInput(session, "view_node", choices=c("Pubkey or alias"="", node_ids), selected=NULL, server=TRUE)
	updateSelectizeInput(session, "subject", choices=c("Pubkey or alias"="", node_ids), selected=NULL, server=TRUE)
	updateSelectizeInput(session, "target", choices=c("Pubkey or alias"="", node_ids), selected=NULL, server=TRUE)
	chan_sim_parms <- reactiveValues()
	status <- reactiveVal()
	observeEvent(input$launch_sim, {
		req(input$subject)
		status('latest')
		subject <- fetch_pubkey(input$subject)
		target <- fetch_pubkey(input$target)
		showModal(modalDialog("Running simulation, please wait...", size='s', footer=NULL))
		sim_graph <- sim_chan(subject, target, channel=input$add_or_del)
		removeModal()
		chan_sim_parms$subject <- subject
		chan_sim_parms$graph <- sim_graph$graph
		chan_sim_parms$betw <- sim_graph$betw
		chan_sim_parms$clo <- sim_graph$clo
		chan_sim_parms$eigen <- sim_graph$eigen
	})
	observeEvent({list(input$subject, input$target)}, {
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
			qualifier <- ifelse(delta>0, 'gain', 'lose')
			color <- ifelse(delta>0, "green", "red")
			val <- paste0(prettyNum(cent.between.rank, big.mark=','), " (", qualifier, " ", abs(delta), ")")
		} else if (input$subject != '') {
			subject <- fetch_pubkey(input$subject)
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
			qualifier <- ifelse(delta>0, 'gain', 'lose')
			color <- ifelse(delta>0, "green", "red")
			val <- paste0(prettyNum(cent.eigen.rank, big.mark=','), " (", qualifier, " ", abs(delta), ")")
		} else if (input$subject != '') {
			subject <- fetch_pubkey(input$subject)
			color <- 'blue'
			val <- prettyNum(g %>% as_tibble %>% filter(name==subject) %>% select(cent.eigen.rank) %>% pull, big.mark=',')
		} else {
			val <- ''
			color <- 'blue'
		}
		valueBox(val, "Eigenvector centrality rank", color=color)
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
			qualifier <- ifelse(delta>0, 'gain', 'lose')
			color <- ifelse(delta>0, "green", "red")
			val <- paste0(prettyNum(cent.close.rank, big.mark=','), " (", qualifier, " ", abs(delta), ")")
		} else if (input$subject != '') {
			subject <- fetch_pubkey(input$subject)
			color <- 'blue'
			val <- prettyNum(g %>% as_tibble %>% filter(name==subject) %>% select(cent.close.rank) %>% pull, big.mark=',')
		} else {
			val <- ''
			color <- 'blue'
		}
		valueBox(val, "Closeness centrality rank", color=color)
	})
	# network-wide centralizations
	output$between.centralization <- renderValueBox({
		if (status() == "latest") {
			delta <- (chan_sim_parms$betw$centralization - g_betw$centralization) / g_betw$centralization * 100
			qualifier <- ifelse(delta>0, '+', '-')
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
			qualifier <- ifelse(delta>0, '+', '-')
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
			qualifier <- ifelse(delta>0, '+', '-')
			val <- paste0(signif(chan_sim_parms$eigen$centralization, 4), " (", qualifier, " ", abs(signif(delta, 2)), "%)")
		} else {
			val <- signif(g_eigen$centralization, 4)
		}
		valueBox(val, "Eigenvector centralization", color='blue')
	})
	# node statistics rendering
	output$userchart <- renderPlotly({
		req(input$chart_x, input$chart_y)
		if ('logx' %in% input$logscale) {
			xscale <- 'log'
		} else {
			xscale <- 'linear'
		}
		if ('logy' %in% input$logscale) {
			yscale <- 'log'
		} else {
			yscale <- 'linear'
		}
		plot_ly(g %>% as_tibble,
			x=as.formula(paste0('~', chart_vars[input$chart_x])),
			y=as.formula(paste0('~', chart_vars[input$chart_y])),
			height='725') %>%
				layout(
					xaxis=list(title=input$chart_x,
					type=xscale), yaxis=list(title=input$chart_y, type=yscale))
	})
	observeEvent(input$chartclear, {
		updateAwesomeCheckboxGroup(session, inputId='logscale', label='log-scale', choices=c('X-axis'='logx', 'Y-axis'='logy'), inline=TRUE)
		updateSelectizeInput(session, inputId='chart_x', label='Choose an X-axis variable', choices=c('', names(chart_vars)))
		updateSelectizeInput(session, inputId='chart_y', label='Choose a Y-axis variable', choices=c('', names(chart_vars)))
	})
}
