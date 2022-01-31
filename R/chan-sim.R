filters <- g %>%
	as_tibble %>%
	summarise(
		max.cap=round(max(tot.capacity)/1e8+1, 0),
		max.avg.capacity=max(avg.capacity)/1e8,
		max.num.channels=max(num.channels)+1,
		max.age=round(max(age)+1, 0),
		max.between=max(cent.between.rank, na.rm=TRUE),
		max.close=max(cent.close.rank, na.rm=TRUE),
		max.eigen=max(cent.eigen.rank, na.rm=TRUE)) %>%
	mutate(max.hops=11) %>%
	t %>%
	as.data.frame %>%
	rownames_to_column()
steps <- c(0.1, 0.01, 1, 1, 1, 1, 1, 1, 1)
min_vals <- c(0.01, 0.001, 0, 1, 0, 1, 1, 1, 0)
med_fee_filter <- c(rowname='max.fee.rate', V1=6000)
filters <- rbind(filters[1:2,], med_fee_filter, filters[-(1:2),])
descr <- c(
	'Filter by range of total capacity (in bitcoin)',
	'Filter by range of average channel capacity (in bitcoin)',
	'Filter by range of average channel fee rates (ppm)',
	'Filter by range of total channels',
	'Filter by range of approximate node age (in days)',
	'Filter by range of betweenness centrality ranks',
	'Filter by range of closeness centrality ranks',
	'Filter by range of eigenvector centrality ranks',
	'Only show nodes that fall within a range of hops away from the node selected in Step 1'
)
filters <- cbind(filters, descr, min_vals, steps)
filters <- filters %>% rename(c('max_vals'='V1', 'filter'='rowname')) %>% t %>% as_tibble

subjectSelectUI <- function(id) {
	selectizeInput(inputId=NS(id, "subject"),
		label=h4(strong('Step 1: enter your pubkey or alias')),
		choices=NULL,
		options=list(placeholder='Pubkey/alias')
	)
}

targetSelectUI <- function(id, targetId, add_or_del) {
	fluidRow(
		column(10, selectizeInput(
			inputId=NS(id, targetId),
			label=NULL,
			choices=NULL,
			options=list(placeholder='Pubkey/alias'))
		),
		column(2, prettyRadioButtons(
			inputId=NS(id, add_or_del),
			label=NULL,
			selected='add',
			choiceNames=c('Add', 'Remove'),
			choiceValues=c('add', 'del'))
		)
	)
}

sliderFilterSelectUI <- function(id, filtId, lab, minVal, maxVal, stepVal) {
	sliderInput(
		inputId=NS(id, filtId),
		label=lab,
		ticks=FALSE,
		min=minVal, max=maxVal, step=stepVal,
		value=c(minVal, maxVal)
	)
}

buttonFilterSelectUI <- function(id, filtId, lab, inLine=TRUE, choice_labels, choice_vals, default_choice) {
	prettyRadioButtons(
		inputId=NS(id, filtId),
		label=lab,
		choiceNames=choice_labels, choiceValues=choice_vals,
		selected=default_choice, inline=inLine
	)
}

dropdownFilterSelectUI <- function(id, filtId, lab, placehold) {
	selectizeInput(inputId=NS(id, filtId),
		label=lab,
		choices=NULL,
		options=list(placeholder=placehold)
	)
}

chansimUI <- function(id) {
	useShinyjs()
	fluidRow(
		column(8,
			box(id=NS(id, 'select.box'), background='yellow', width=NULL,
				fluidRow(
					column(12,
						subjectSelectUI(NS(id, 'subject_select'))
					),
				),

				h4(p(strong('Step 2: enter or select pubkey/alias of up to 3 nodes with which to simulate adding or removing channels'))),
				lapply(c(1:3), function(x)
					targetSelectUI(
						NS(id, 'target_select'),
						paste0('target', x),
						paste0('add_or_del', x)
					)
				),

				box(id=NS(id, "filt.box"), title=uiOutput(NS(id, 'targets_num')),
					background="yellow", width=NULL, collapsible=TRUE,
					collapsed=TRUE, solidHeader=TRUE, status='primary',

					lapply(filters, function(x)
						sliderFilterSelectUI(
							id=NS(id, 'filters'),
							filtId=x[1], lab=x[3],
							minVal=as.numeric(x[4]), maxVal=as.numeric(x[2]),
							stepVal=as.numeric(x[5]))
					),
					buttonFilterSelectUI(
						id=NS(id, 'filters'),
						filtId='peers.of.peers',
						lab='Filter out nodes that have at least one peer in common with the node selected in Step 1',
						inLine=TRUE,
						choice_labels=c('Yes', 'No'), choice_vals=c(1, 2),
						default_choice=2
					),
					shinyjs::hidden(
						dropdownFilterSelectUI(
							id=NS(id, 'filters'),
							filtId='community',
							lab='Filter nodes by Amboss community',
							placehold='Community'
						)
					)
				),
				column(12, align='center',
					startButtonUI(NS(id, 'launch_sim'))
				),
			),
			tabBox(
				id=NS(id, 'chansim_peer_stats'),
				side='left', width=NULL,
				selected='chansim_venn_tab',
				tabPanel(
					id=NS(id, 'chansim_venn_tab'),
					title='Peer overlap', width=NULL,
					value='chansim_venn_tab',
					peerOverlapUI(NS(id, 'chansim_venn')),
				) 
			),
		),
		column(4,
			fluidRow(
				box(id=NS(id, 'cent.box'), title="Node centrality ranks",
					solidHeader=TRUE, collapsible=TRUE, width=NULL,
					centralityUI(NS(id, 'cents'))
				)
			)
		)
	)

}

peerOverlapUI <- function(id) {
	withSpinner(
		plotlyOutput(NS(id, 'chansim_venn'))
	)
}

startButtonUI <- function(id) {
	actionBttn(
		inputId=NS(id, 'launch_sim_button'),
		label='Start',
		style='fill',
		color='success',
		block=FALSE
	)

}

centralityUI <- function(id) {
	tagList(
		valueBoxOutput(NS(id, 'cent.between'), width=12) %>% bs_embed_tooltip(title="Betweenness centrality measures how many shortest paths a node sits in between any two other nodes. Higher ranking nodes tend to be in more shortest paths between other nodes and are thus more likely to be in a potential route.", placement='top'),
		valueBoxOutput(NS(id, 'cent.eigen'), width=12) %>% bs_embed_tooltip(title="Eigenvector/hubness centrality is a node's influence in the network. Higher ranking nodes tend to have more channels, and are also connected to other high ranking nodes who themselves have many channels.", placement='top'),
		valueBoxOutput(NS(id, 'cent.close'), width=12) %>% bs_embed_tooltip(title="Closeness/hopness centrality measures the distance from a node to any other in the network. Higher ranking nodes have to make fewer hops to reach any other node on the network.", placement='top')
	)
}

resetFiltersServer <- function(id, subject, db=con) {
	moduleServer(id, function(input, output, session) {
		observeEvent(subject(), {
			lapply(filters, function(x)
				updateSliderInput(session, inputId=x[1], value=c(x[4], x[2]))
			)
			updateSliderInput(session, inputId='max.hops', value=c(0, 11))
			updateSliderInput(session, inputId='peers.of.peers', value=2)

			comms <- tbl(con, 'communities') %>% pull(community) %>% unique %>% sort
			updateSelectizeInput(session, inputId="community",
				choices=c("Community"=NULL, comms),
				selected=character(0),
				server=TRUE
			)
		})
	})
}

centralityRankServer <- function(id, graph=g, subject, metric, sim_output=NULL) {
	moduleServer(id, function(input, output, session) {
		result_metric <- paste0('cent.', metric, '.rank')
		label <- ifelse(metric == "between", "Betweenness",
			ifelse(metric == "close", "Closeness/hopness",
			"Eigenvector/hubness"))
		if (!is.null(sim_output)) {
			data <- sim_output()$graph %>%
				filter(name==sim_output()$subject) %>%
				dplyr::select(paste0('sim.cent.', metric, '.rank'), paste0('cent.', metric, '.rank.delta')) %>%
				as_tibble
			cent.rank <- eval(parse(text=paste0('data$sim.cent.', metric, '.rank')))
			delta <- eval(parse(text=paste0('data$cent.', metric, '.rank.delta')))
			qualifier <- ifelse(delta>0, 'gain', ifelse(delta==0, 'no change', 'lose'))
			color <- ifelse(delta>0, "green", ifelse(delta==0, "blue", "red"))
			val <- ifelse(delta==0,
				paste0(prettyNum(cent.rank, big.mark=','), ' (', qualifier, ')'),
				paste0(prettyNum(cent.rank, big.mark=','), " (", qualifier, " ", abs(delta), ")"))
		} else if (subject() != '') {
			color <- 'blue'
			cent.rank <- graph %>%
				as_tibble %>%
				filter(name==subject()) %>% 
				pull(result_metric)
			val <- prettyNum(cent.rank, big.mark=',')
		} else {
			val <- ''
			color <- 'blue'
		}
		return(reactive(list(value=val, metric=metric, label=label, color=color)))
	})
}

centralityRankOutput <- function(id, data) {
	moduleServer(id, function(input, output, session) {
		output[[paste0('cent.', data()$metric)]] <- renderValueBox({
			valueBox(data()$value, paste(data()$label, "centrality rank"), color=data()$color)
		})
	})
}

peerOverlapServer <- function(id, subject, targets) {
	moduleServer(id, function(input, output, session) {
		output$chansim_venn <- renderPlotly({
			req(subject() != "")
			req(length(targets()[targets() != ""]) > 0)
			xkey <- subject()
			xkey_alias <- fetch_alias(xkey)
			ykeys <- targets()[targets() != ""]
			ykey_aliases <- sapply(ykeys, function(z) fetch_alias(z)) %>% as.vector
			peers <- lapply(c(xkey, ykeys), function(z) fetch_peer_aliases(z))
			names(peers) <- c(xkey_alias, ykey_aliases)
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
				plotly::layout(xaxis = ax, yaxis = ax, hovermode='compare')
		})
	})
}

channelSimulationServer <- function(id, subject, targets, add_or_del) {
	moduleServer(id, function(input, output, session) {
		targets <- targets()[targets() != ""]
		indels <- add_or_del()[which(targets() != '')]
		showModal(modalDialog("Running simulation, please wait...", size='s', footer=NULL))
		sim_graph <- simulate_channel(subject(), targets, indels)
		removeModal()
		return(
			reactive(
				list(
					subject=subject(),
					graph=sim_graph$graph,
					betw=sim_graph$betw,
					clo=sim_graph$clo,
					eigen=sim_graph$eigen
				)
			)
		)
	})
}

applyFiltersToTargetsServer <- function(id, graph=g, pubkey, node_list=node_ids, db=con) {
	moduleServer(id, function(input, output, session) {
		if (pubkey != "") {
			vals <- make_ego_graph(graph, order=input$max.hops[2]+1, nodes=pubkey, mindist=input$max.hops[1]+1)[[1]] %>%
				as_tbl_graph %>%
				filter(
					tot.capacity>=input$max.cap[1]*1e8, tot.capacity<=input$max.cap[2]*1e8,
					avg.capacity>=input$max.avg.capacity[1]*1e8, avg.capacity<=input$max.avg.capacity[2]*1e8,
					num.channels>=input$max.num.channels[1], num.channels<=input$max.num.channels[2],
					median.rate.ppm>=input$max.fee.rate[1], median.rate.ppm<=input$max.fee.rate[2],
					age>=input$max.age[1], age<=input$max.age[2],
					cent.between.rank>=input$max.between[1], cent.between.rank<=input$max.between[2],
					cent.close.rank>=input$max.close[1], cent.close.rank<=input$max.close[2],
					cent.eigen.rank>=input$max.eigen[1], cent.eigen.rank<=input$max.eigen[2]) %>%
				mutate(target=paste(alias, "-", name))
				if (input$community != "") {
					members <- tbl(con, 'communities') %>%
						filter(community==local(input$community)) %>%
						pull(pubkey)
					vals <- vals %>% filter(name %in% members)
				}
			if (as.numeric(input$peers.of.peers) == 1) {
				peers_of_peers <- fetch_peers_of_peers(pubkey) %>% unlist %>% unique
				vals <- vals %>% as_tibble %>% filter(!(alias %in% peers_of_peers)) %>% pull(target)
			} else {
				vals <- vals %>% as_tibble %>% pull(target)
			}
			return(vals)
		}
	})
}

renderFilterBarServer <- function(id, targets_list) {
	moduleServer(id, function(input, output, session) {
		renderUI({
			if (length(targets_list) > 0) {
				return(paste("Optional: expand this bar to apply filters to the", length(targets_list), "nodes in the drop-down menus in Step 2"))
			} else {
				return(paste("Optional: expand this bar to apply filters to nodes in the drop-down menus in Step 2"))
			}
		})

	})
}

subjectSelectServer <- function(id, pubkey_list=node_ids) {
	moduleServer(id, function(input, output, session) {
		updateSelectizeInput(
			session, 
			inputId="subject",
			choices=c("Pubkey or alias"=NULL, pubkey_list),
			selected=character(0),
			server=TRUE)
	})
}

targetUpdateServer <- function(id, pubkey_list) {
	moduleServer(id, function(input, output, session) {
		lapply(c(1:3), function(x)
			updateSelectizeInput(
				session,
				inputId=paste0('target', x),
				choices=c("Pubkey or alias"="", pubkey_list),
				selected=character(0),
				server=TRUE
			)
		)
	})
}

dropdownFilterSelectServer <- function(id, db=con, logged_in=FALSE) {
	moduleServer(id, function(input, output, session) {
		shiny::observe({
			shinyjs::toggle('community', condition=logged_in())
		})
		comms <- tbl(con, 'communities') %>% pull(community) %>% unique %>% sort
		updateSelectizeInput(
			session, 
			inputId="community",
			choices=c("Community"=NULL, comms),
			selected=character(0),
			server=TRUE
		)
	})
}

getSubject <- function(id) {
	moduleServer(id, function(input, output, session) {
		reactive(fetch_pubkey(input$subject))
	})
}

getTargets <- function(id) {
	moduleServer(id, function(input, output, session) {
		reactive(
			sapply(
				c(1:3), 
				function(x)
					fetch_pubkey(eval(parse(text=paste0('input$target', x))))
			)
		)
	})
}

getChannelActions <- function(id) {
	moduleServer(id, function(input, output, session) {
		reactive(
			sapply(
				c(1:3), 
				function(x)
					eval(parse(text=paste0('input$add_or_del', x)))
			)
		)
	})
}

startButtonServer <- function(id) {
	moduleServer(id, function(input, output, session) {
		reactive(input$launch_sim_button)
	})
}

chansimServer <- function(id, reactive_show) {
	moduleServer(id, function(input, output, session) {
		subject <- getSubject('subject_select')
		targets <- getTargets('target_select')
		target_choices <- reactiveVal()
		sim_output <- reactiveValues()
		channel_actions <- getChannelActions('target_select')
		launch_sim <- startButtonServer('launch_sim')

		observe({
			target_choices <- applyFiltersToTargetsServer('filters', pubkey=subject())
			output$targets_num <- renderFilterBarServer('filterbar', target_choices)
			output$chansim_venn <- peerOverlapServer('chansim_venn', subject, targets)

			targetUpdateServer('target_select', target_choices)

			centralities <- lapply(
				c('between', 'close', 'eigen'),
				function(x)
					centralityRankServer('cents', g, subject, x)
				)
			lapply(centralities, function(x) centralityRankOutput('cents', x))
		})

		dropdownFilterSelectServer('filters', db=con, logged_in=reactive_show)
		resetFiltersServer('filters', subject)
		subjectSelectServer('subject_select')

		observeEvent(launch_sim(), {
			req(subject() != "")
			req(length(unique(targets())) != 1 && unique(targets()) != "")
			sim_output <- channelSimulationServer('launch_sim', subject, targets, channel_actions)

			simulated_centralities <- lapply(
				c('between', 'close', 'eigen'),
				function(x)
					centralityRankServer('cents', g, subject, x, sim_output)
				)
			lapply(simulated_centralities, function(x) centralityRankOutput('cents', x))
		})
	})

}

chansimApp <- function() {
  
	ui <- dashboardPage(
		dashboardHeader(title='Channel Simulator'),
		dashboardSidebar(),
		dashboardBody(chansimUI('x')),
		skin='yellow',
	)
	server <- function(input, output, session) {
		chansimServer('x', reactive_show=reactive(FALSE))
	}
	shinyApp(ui, server)
  
}
