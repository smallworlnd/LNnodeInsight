source("inst/shiny-common.R")

chansim_filters <- data.frame(
	filter_vars=c("max.cap", "max.avg.capacity", "max.fee.rate.out", "max.fee.rate.in", "max.num.channels", "max.between", "max.close", "max.eigen", "max.hops", "max.lnplus.rank"),
	filter_max=tbl(pool, 'nodes_current') %>%
		summarise(
			max.cap=round(max(tot.capacity)/1e8+1, 0),
			max.avg.capacity=max(avg.capacity)/1e8,
			max.fee.rate.out=6000,
			max.fee.rate.in=6000,
			max.num.channels=max(num.channels)+1,
			max.between=max(cent.between.rank),
			max.close=max(cent.close.rank),
			max.eigen=max(cent.eigen.rank),
			max.hops=11,
			max.lnplus.rank=10) %>%
		as_tibble %>%
		unlist(use.names=FALSE),
	filter_descr=c(
		'Filter by range of total capacity (in BTC)',
		'Filter by range of average channel capacity (in BTC)',
		'Filter by range of average outbound channel fee rates (ppm)',
		'Filter by range of average inbound channel fee rates (ppm)',
		'Filter by range of total channels',
		'Filter by range of betweenness centrality ranks',
		'Filter by range of closeness centrality ranks',
		'Filter by range of eigenvector centrality ranks',
		'Only show nodes that fall within a range of hops away from the node selected in Step 1',
		"Filter by range of LightningNetwork+ ranks"),
	filter_min=c(0.01, 0.001, 0, 0, 1, 1, 1, 1, 0, 1),
	filter_steps=c(0.1, 0.01, 1, 1, 1, 1, 1, 1, 1, 1)
	) %>% t %>% as.data.frame


#' UI element for node operator's pubkey selection
#'
#' TODO: replace with generic UI element like \link{nodeSelectUI}
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return return selectize UI for pubkey selection
#' @export
subjectSelectUI <- function(id) {
	selectizeInput(inputId=NS(id, "subject"),
		label=h4(strong('Step 1: enter your pubkey or alias')),
		choices=NULL,
		options=list(placeholder='Pubkey/alias')
	)
}

#' UI element for node operator to select other pubkeys with which to simulate
#' channels
#'
#' TODO: replace with generic UI element like \link{nodeSelectUI}
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param targetId 
#' @return return selectize UI for target pubkey selections and whether they are
#' added or removed
#' @export
targetSelectUI <- function(id, targetId, add_or_del, clip_pubkey) {
	fluidRow(
		column(9, selectizeInput(
			inputId=NS(id, targetId),
			label=NULL,
			choices=NULL,
			options=list(placeholder='Pubkey/alias'))
		),
		column(1, align="center", uiOutput(NS(id, clip_pubkey))),
		column(1, align="left", prettyRadioButtons(
			inputId=NS(id, add_or_del),
			label=NULL,
			selected='add',
			choiceNames=c('Add', 'Remove'),
			choiceValues=c('add', 'del'))
		)
	)
}

#' UI numeric range element for the various node filters
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param filtId filter-specific short ID
#' @param lab descriptive label for the ui element
#' @param minVal minimum value for the filter
#' @param maxVal maximum value for the filter
#' @param stepVal step increments
#' @return return filt UI element
#' @export
numRangeFilterSelectUI <- function(id, filtId, lab, minVal, maxVal, stepVal) {
	numericRangeInput(
		inputId=NS(id, filtId),
		label=lab,
		min=minVal, max=maxVal, step=stepVal,
		value=c(minVal, maxVal)
	)
}


#' button UI element for the various node filters
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param filtId filter-specific short ID
#' @param lab descriptive label for the ui element
#' @param inLine whether to display buttons in-line or not
#' @param choice_labels descriptive labels of the buttons
#' @param default_choice default button choice
#' @return return button filt UI element
#' @export
buttonFilterSelectUI <- function(id, filtId, lab, inLine=TRUE, choice_labels, choice_vals, default_choice) {
	prettyRadioButtons(
		inputId=NS(id, filtId),
		label=lab,
		choiceNames=choice_labels, choiceValues=choice_vals,
		selected=default_choice, inline=inLine
	)
}

#' dropdown filter UI element
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param filtId filter-specific short ID
#' @param lab descriptive label for the ui element
#' @param placehold placeholder text
#' @return return dropdown filter UI element
#' @export
dropdownFilterSelectUI <- function(id, filtId, lab, placehold) {
	selectizeInput(inputId=NS(id, filtId),
		label=lab,
		choices=NULL,
		options=list(placeholder=placehold)
	)
}

#' main layout UI for various app elements
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return return the main layout UI element
#' @export
chansimUI <- function(id) {
	useShinyjs()
	ns <- NS(id)
	fluidRow(
		column(8,
			box(
				fluidRow(
					column(12,
						subjectSelectUI(NS(id, 'subject_select'))
					),
				),

				h4(p(strong('Step 2: enter or select pubkey/alias of up to 3 nodes with which to simulate adding or removing channels'))),
				h5(p(strong("*Existing peers of the node selected in Step 1 will only appear if 'Remove' is selected"))),
				rclipboardSetup(),
				lapply(c(1:3), function(x)
					targetSelectUI(
						NS(id, 'target_select'),
						paste0('target', x),
						paste0('add_or_del', x),
						paste0('clip_pubkey', x)
					)
				),

				box(id=NS(id, "filt.box"), title=uiOutput(NS(id, 'targets_num')),
					background="yellow", width=NULL, collapsible=TRUE,
					collapsed=TRUE, solidHeader=TRUE, status='primary',

					lapply(
						chansim_filters,
						function(x)
							numRangeFilterSelectUI(
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
						buttonFilterSelectUI(
							id=NS(id, 'filters'),
							filtId='network.addr',
							lab='Filter for nodes by connection type (IPV4/IPV6/Tor)',
							inLine=TRUE,
							choice_labels=c('All types', 'IPV4/IPV6 only', 'Hybrid IPV4/IPV6/Tor', 'Tor only'), choice_vals=c('all', 'clearnet', 'hybrid', 'tor'),
							default_choice='all'
						)
					),
					shinyjs::hidden(
						dropdownFilterSelectUI(
							id=NS(id, 'filters'),
							filtId='community',
							lab='Filter nodes by Amboss community',
							placehold='Community'
						)
					),
					shinyjs::hidden(
						buttonFilterSelectUI(
							id=NS(id, 'filters'),
							filtId='lnplus_pending',
							lab='Filter for nodes currently participating in pending LN+ swaps',
							inLine=TRUE,
							choice_labels=c('Yes', 'No'), choice_vals=c(1, 2),
							default_choice=2
						)
					),
					shinyjs::hidden(
						numRangeFilterSelectUI(
							id=NS(id, 'filters'),
							filtId="lnplus_rank", lab="Filter for nodes participating in pending swaps by LN+ rank (filtered by swaps that the pubkey selected in Step 1 is eligible for)",
							minVal=1, maxVal=10, stepVal=1)
					)
				),
				column(12, align='center',
					startButtonUI(NS(id, 'launch_sim'), lab="Start")
				),
				background='yellow', width=NULL
			),
			tabBox(
				id=NS(id, 'chansim_peer_stats'),
				side='left', width=NULL,
				selected='chansim_venn_tab',
				tabPanel(
					id=NS(id, 'chansim_venn_tab'),
					title='Peer overlap', width=NULL,
					value='chansim_venn_tab',
					peerVennUI(NS(id, 'chansim_venn')),
				) 
			),
		),
		column(4,
			fluidRow(
				box(centralityUI(NS(id, 'cents')),
					title="Node centrality ranks", solidHeader=TRUE, collapsible=TRUE, width=12
				),
			)
		),
		conditionalPanel(
			"output.previous_results == 1", ns=ns,
			column(4,
				fluidRow(
					box(
						pastRunUI(NS(id, "past_action")),
						br(),
						centralityUI(NS(id, "past_result")),
						title="Previous run results",
						solidHeader=TRUE, collapsible=TRUE, width=NULL
					)
				)
			)
		),
		conditionalPanel(
			"output.filter_by_lnplus_swaps == 1", ns=ns,
			column(4, 
				uiOutput(NS(id, "show_qualifying_swaps"))
			)
		),
		column(4, align="center",
			conditionalPanel(
				"output.account_is_premium == 'false'", ns=ns,
				upgradeButtonUI(NS(id, "ad_upgrade"))
			)
		),
	)

}

#' past run UI
#'
#' UI element identical to simulation results but meant to show previous run
#' results
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
pastRunUI <- function(id) {
	uiOutput(NS(id, "target_actions"))
}


#' UI element for node operator to select other pubkeys with which to simulate
#' channels
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param targetId 
#' @return return selectize UI for target pubkey selections and whether they are
#' added or removed
#' @export
peerVennUI <- function(id) {
	withSpinner(
		plotlyOutput(NS(id, 'chansim_venn'))
	)
}

#' UI elements to display centrality rank
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return return value box UI elements for the three centrality ranks
#' @export
centralityUI <- function(id) {
	tagList(
		valueBoxOutput(NS(id, 'cent.between'), width=12) %>% bs_embed_tooltip(title="Betweenness centrality measures how many shortest paths a node sits in between any two other nodes. Higher ranking nodes tend to be in more shortest paths between other nodes and are thus more likely to be in a potential route.", placement='top'),
		valueBoxOutput(NS(id, 'cent.eigen'), width=12) %>% bs_embed_tooltip(title="Eigenvector/hubness centrality is a node's influence in the network. Higher ranking nodes tend to have more channels, and are also connected to other high ranking nodes who themselves have many channels.", placement='top'),
		valueBoxOutput(NS(id, 'cent.close'), width=12) %>% bs_embed_tooltip(title="Closeness/hopness centrality measures the distance from a node to any other in the network. Higher ranking nodes have to make fewer hops to reach any other node on the network.", placement='top')
	)
}

#' target pubkey clipboard
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param subject reactively reset filters on subject pubkey change
#' @param filters data.frame of peer filters specified in global.R
#' @param db backend database for accessing amboss community list
#' @return reset filters
#' @export
pubkeyClipboardServer <- function(id, targets) {
	moduleServer(id, function(input, output, session) {
		lapply(seq_along(targets),
			function(x)
			output[[paste0("clip_pubkey", x)]] <- renderUI({
				rclipButton(
					inputId=paste0("clip_pubkey", x),
					label=NULL,
					clipText=targets[x],
					icon=icon('clipboard')
				)
			})
		)
	})
}

#' pubkey filter resetting server
#'
#' module for resetting \link{numRangeFilterSelectUI},
#' \link{dropdownFilterSelectUI}, \link{buttonFilterSelectUI}
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param subject reactively reset filters on subject pubkey change
#' @param filters data.frame of peer filters specified in global.R
#' @param db backend database for accessing amboss community list
#' @return reset filters
#' @export
resetFiltersServer <- function(id, subject, filters=chansim_filters, db=pool) {
	moduleServer(id, function(input, output, session) {
		observeEvent(subject(), {
			lapply(chansim_filters, function(x)
				updateNumericRangeInput(session, inputId=x[1], value=as.numeric(c(x[4], x[2])))
			)
			updateNumericRangeInput(session, inputId='max.hops', value=c(0, 11))
			updatePrettyRadioButtons(session, inputId='peers.of.peers', selected=2)
			updatePrettyRadioButtons(session, inputId='lnplus_pending', selected=2)
			updatePrettyRadioButtons(session, inputId='network.addr', selected='all')
			updateNumericRangeInput(session, inputId='lnplus_rank', value=c(1, 10))

			comms <- tbl(db, 'communities') %>%
				distinct(community) %>%
				pull(community) %>%
				sort
			updateSelectizeInput(session, inputId="community",
				choices=c("Community"=NULL, comms),
				selected=character(0),
				server=TRUE
			)
		})
	})
}

#' centrality rank server
#'
#' module for fetching current/modified centrality ranks of the subject node,
#' depending on whether a simulation was recently run
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param graph (undirected) graph from which to pull node centrality rank
#' variables
#' @param subject subject pubkey to pull centrality rank information
#' @param metric the given centrality metric, chosen from 'between', 'close' or
#' 'eigen'
#' @param sim_results_reactive reactive graph from a simulation run which includes the
#' modified centralities
#' @return return centrality ranks, either current or simulated
#' @export
centralityRankServer <- function(id, graph=undir_graph, subject, metric, sim_results_reactive=NULL) {
	moduleServer(id, function(input, output, session) {
		result_metric <- paste0('cent.', metric, '.rank')
		label <- ifelse(metric == "between", "Betweenness",
			ifelse(metric == "close", "Closeness/hopness",
			"Eigenvector/hubness"))
		if (!is.null(sim_results_reactive)) {
			data <- sim_results_reactive %>%
				filter(pubkey==subject())
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
				filter(pubkey==subject()) %>% 
				pull(result_metric)
			val <- prettyNum(cent.rank, big.mark=',')
		} else {
			val <- ''
			color <- 'blue'
		}
		return(reactive(list(value=val, metric=metric, label=label, color=color)))
	})
}

#' value box output server for a given centrality rank
#'
#' module for fetching the current/simulated centrality rank
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param data output of \link{centralityRankServer}
#' @return return centrality ranks, either current or simulated
#' @export
centralityRankOutput <- function(id, data) {
	moduleServer(id, function(input, output, session) {
		output[[paste0('cent.', data()$metric)]] <- renderValueBox({
			valueBox(data()$value, paste(data()$label, "centrality rank"), color=data()$color)
		})
	})
}

#' Venn diagram server
#'
#' module for computing peer overlaps between subject and targets and wrapping
#' into a plotly output element
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param subject subject pubkey with which we find overlaps for
#' @param targets chosen for finding overlaps
#' @return return ggplot Venn diagram of peer overlaps
#' @export
peerVennServer <- function(id, subject, targets, nodes=nodes_current) {
	moduleServer(id, function(input, output, session) {
		output$chansim_venn <- renderPlotly({
			req(subject() != "")
			req(length(targets()[targets() != ""]) > 0)
			xkey <- subject()
			xkey_alias <- fetch_alias(pubkey=xkey)
			ykeys <- targets()[targets() != ""]
			ykey_aliases <- as.vector(sapply(ykeys, function(x) nodes$alias[nodes$pubkey==x]))
			peers <- lapply(c(xkey, ykeys), function(z) fetch_peer_aliases(pubkey=z))
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


#' channel simulation server
#'
#' module for computing the impact on centralities from adding or removing
#' target channels to the subject
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param subject subject pubkey on which simulations are run
#' @param targets vector of target nodes with which we're adding/removing channels
#' @param add_or_del vector of choices whether to add/remove
#' @param api_info url and auth token (if present) of channel simulation
#' api/backend
#' @return return a data.frame/tibble from the graph containing the simulation
#' results
#' @export
channelSimulationServer <- function(id, subject, targets, add_or_del, api_info) {
	moduleServer(id, function(input, output, session) {
		subject <- subject()
		targets <- targets()[targets() != ""]
		indels <- add_or_del()[which(targets() != '')]

		# poorman's async request to chansim api by sending process to background
		# and avoid session locks
		sim_request <- callr::r_bg(
			func = function(subject_pubkey, target_pubkeys, indel, api_info) {
				req_body <- jsonlite::toJSON(
					list(subject_pubkey=subject_pubkey, target_pubkeys=target_pubkeys, indel=indel),
					auto_unbox=TRUE)
				if ("token" %in% names(api_info)) {
					api_request <- googleCloudRunner::cr_jwt_with_httr(
						httr::POST(url=api_info$url, body=req_body, encode="json"),
						api_info$token)
				} else {
					api_request <- httr::POST(url=api_info$url, body=req_body, encode="json")
				}
				return(httr::content(api_request))
			},
			args=list(subject_pubkey=subject, target_pubkeys=targets, indel=indels, api_info=api_info),
			supervise=TRUE
		)
		return(sim_request)
	})
}

#' filter node pubkey server
#' 
#' reactively filter node pubkey lists based on buttons, sliders and dropdowns
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param graph graph object in order to apply the hops filter on the subject
#' node
#' @param pubkey pubkey of the subject node for which we apply filters
#' @param db db connection for accessing commmunities (maybe should just be a
#' vector of communities rather than a db connection?)
#' @return returns filtered list of node pubkeys for choosing from
#' @export
applyFiltersToTargetsServer <- function(id, graph=undir_graph, pubkey, node_list=node_ids, db=pool) {
	moduleServer(id, function(input, output, session) {
		if (pubkey != "") {
			# apply user-defined filters
			vals <- make_ego_graph(graph, order=input$max.hops[2]+1, nodes=fetch_id(pubkey=pubkey), mindist=input$max.hops[1]+1)[[1]] %>%
				as_tbl_graph %>%
				filter(
					tot.capacity>=input$max.cap[1]*1e8, tot.capacity<=input$max.cap[2]*1e8,
					avg.capacity>=input$max.avg.capacity[1]*1e8, avg.capacity<=input$max.avg.capacity[2]*1e8,
					num.channels>=input$max.num.channels[1], num.channels<=input$max.num.channels[2],
					median.rate.ppm.out>=input$max.fee.rate.out[1], median.rate.ppm.out<=input$max.fee.rate.out[2],
					median.rate.ppm.in>=input$max.fee.rate.in[1], median.rate.ppm.in<=input$max.fee.rate.in[2],
					cent.between.rank>=input$max.between[1], cent.between.rank<=input$max.between[2],
					cent.close.rank>=input$max.close[1], cent.close.rank<=input$max.close[2],
					cent.eigen.rank>=input$max.eigen[1], cent.eigen.rank<=input$max.eigen[2]) %>%
				mutate(target=paste(alias, "-", pubkey))
			# apply amboss community filter if user selected
			if (input$community != "") {
				members <- tbl(db, 'communities') %>%
					filter(community==local(input$community)) %>%
					pull(pubkey)
				vals <- vals %>% filter(pubkey %in% members)
			}
			# apply peers of peers filter if user selected
			if (as.numeric(input$peers.of.peers) == 1) {
				peers_of_peers <- fetch_peers_of_peers(pubkey=pubkey) %>% unlist %>% unique
				vals <- vals %>% as_tibble %>% filter(!(alias %in% peers_of_peers))
			}
			# apply lnplus pending swap node filter if user selected
			if (as.numeric(input$lnplus_pending) == 1) {
				pubkey_stats <- as_tibble(graph) %>%
					filter(pubkey==!!pubkey)
				nodes_in_swaps <- tbl(db, "lnplus_pending") %>%
					filter(
						lnplus_rank_number>=!!input$lnplus_rank[1],
						lnplus_rank_number<=!!input$lnplus_rank[2],
						min_cap<=!!pubkey_stats$tot.capacity,
						min_channels<=!!pubkey_stats$num.channels) %>%
					as_tibble %>%
					pull(pubkey) %>%
					unique
				vals <- vals %>% filter(pubkey %in% nodes_in_swaps)
			}
			if (input$network.addr == 'clearnet') {
				vals <- vals %>% filter(is.na(torv3))
			}
			else if (input$network.addr == 'hybrid') {
				vals <- vals %>% filter(!is.na(ipv4) | !is.na(ipv6))
			}
			else if (input$network.addr == 'tor') {
				vals <- vals %>% filter(is.na(ipv4) & is.na(ipv6))
			}
			return(pull(vals, target))
		}
	})
}

#' dropdown filter bar text server
#'
#' renders text for the collapsible filter bar to display the number of
#' resulting node pubkeys after applying filters
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param targets_list resulting list pubkeys after filters are applied
#' @return returns reactive text on the number of nodes available for selection
#' @export
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

#' subject selection server
#'
#' server-side pubkey list server to select from for subject
#' TODO: replace with generic \link{nodeListServer}
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param pubkey_list list of pubkeys in the form "alias - pubkey" from which to
#' select
#' @return returns list of pubkeys
#' @export
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

#' target selection server
#'
#' server-side pubkey list server to select from for targets
#' TODO: replace with generic \link{nodeListServer}
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param pubkey_list list of pubkeys in the form "alias - pubkey" from which to
#' select
#' @return returns list of pubkeys for up to 3 targets
#' @export
targetUpdateServer <- function(id, selector_number, pubkey_list, previous_selection, graph=undir_graph) {
	moduleServer(id, function(input, output, session) {
		updateSelectizeInput(
			session,
			inputId=paste0('target', selector_number),
			choices=c("Pubkey or alias"="", pubkey_list),
			selected=undir_graph %>% filter(pubkey==previous_selection) %>% mutate(alias_pubkey=paste0(alias, " - ", pubkey)) %>% pull(alias_pubkey),
			server=TRUE
		)
	})
}

#' toggle hidden elements depending on account status
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials make community filter accessible if user is logged in
#' @return returns toggled/hidden UI elements depending on account status
#' @export
toggleHiddenElements <- function(id, credentials) {
	moduleServer(id, function(input, output, session) {
		shiny::observe({
			req(credentials()$user_auth)
			shinyjs::toggle('network.addr')
			shinyjs::toggle('community')
			shinyjs::toggle('lnplus_pending')
		})
		shiny::observeEvent(input$lnplus_pending, {
			if (input$lnplus_pending == 1) {
				shinyjs::toggle('lnplus_rank', condition=credentials()$user_auth)
			} else {
				shinyjs::hide('lnplus_rank')
			}
		})
	})
}

#' LN+ swap UI element
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param qualifying_swaps swaps a node is eligible for
#' @return returns valueboxes summarizing all eligible swaps a target pubkey is
#' currently participating in
#' @export
lnplusSwapUIServer <- function(id, qualifying_swaps) {
	moduleServer(id, function(input, output, session) {
		output$qualifying_swaps <- renderUI({
			box(
				lapply(
					qualifying_swaps %>% collect %>% t %>% as.data.frame,
					function(x)
						a(href=x[15], target="_blank",
							valueBox(
								paste(prettyNum(x[12], big.mark=","), "sats"), paste0("Swap #", x[13]),
								icon=icon("exchange-alt"), color="purple", width=12
							)
						)
				),
				title="Open swaps on LN+", width=NULL, collapsible=TRUE
			)
		})
	})
}

#' LN+ filter activation reactive
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns input reactive if LN+ filter is selected
#' @export
lnplusFilterActivate <- function(id) {
	moduleServer(id, function(input, output, session) {
		reactive(input$lnplus_pending)
	})
}


#' dropdown filter selection server
#'
#' module for dropdown menu filters, like for filtering by amboss community
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param db db from which we pull community information (maybe should be a
#' vector instead)
#' @param credentials make community filter accessible if user is logged in
#' @return returns list of amboss communities from which to filter target node
#' pubkeys
#' @export
dropdownFilterSelectServer <- function(id, db=pool) {
	moduleServer(id, function(input, output, session) {
		comms <- tbl(db, 'communities') %>% pull(community) %>% unique %>% sort
		updateSelectizeInput(
			session, 
			inputId="community",
			choices=c("Community"=NULL, comms),
			selected=character(0),
			server=TRUE
		)
	})
}

#' get target pubkey
#'
#' module to extract pubkey from "alias - pubkey" string
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns pubkey string
#' @export
getTargets <- function(id) {
	moduleServer(id, function(input, output, session) {
		reactive(
			sapply(c(1:3), 
				function(x) fetch_pubkey(eval(parse(text=paste0('input$target', x))))
			)
		)
	})
}


#' get add/del channel choice
#'
#' module to fetch user choice of channel add/delete
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns add/del string associated per target pubkey in
#' \link{getTargets}
#' @export
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

#' format choices of nodes and actions
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param targets vector of nodes chosen for simulating channels with
#' @param actions vector of choices to add/remove
#' @return returns renderText object of formatted aliass + actions
#' @export
formatUserChoices <- function(id, targets, actions, nodes=nodes_current) {
	moduleServer(id, function(input, output, session) {
		output$target_actions <- renderText({
			aliases <- nodes %>% filter(pubkey %in% targets[targets != ""]) %>% pull(alias)
			indels <- str_replace(actions()[targets != ""], "del", "remove")
			paste("<b>", str_to_title(indels), aliases, "</b><br>")
		})
	})
}

#' main channel simulation server
#'
#' backend handling of all inputs/outputs for channel simulation
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials reactively hide/show some filters depending on user
#' account
#' @param api_info api url and auth token to POST channel simulation information
#' to local (if no auth token present) or remote backend
#' @export
chansimServer <- function(id, api_info, credentials, db=pool) {
	moduleServer(id, function(input, output, session) {
		# initializ reactive values
		pending_swaps <- tbl(db, "lnplus_pending")
		subject <- getNodePubkey('subject_select', "subject")
		targets <- getTargets('target_select')
		observeEvent(targets(), {
			pubkeyClipboardServer("target_select", targets())
		})
		available_choices <- reactiveVal()
		channel_actions <- getChannelActions('target_select')
		output$chansim_venn <- peerVennServer('chansim_venn', subject, targets)
		previous_targets <- reactiveVal(NULL)

		# adapt target lists on observing any filter changes
		observe({
			available_choices <- applyFiltersToTargetsServer('filters', pubkey=subject())
			output$targets_num <- renderFilterBarServer('filterbar', available_choices)
			lapply(
				seq(channel_actions()),
				function(x) {
					peer_ids <- adjacent_vertices(undir_graph, fetch_id(pubkey=subject()), mode='all') %>% unlist
					peer_alias_pubkeys <- undir_graph %>%
						filter(id %in% peer_ids) %>%
						mutate(alias_pubkey=paste0(alias, " - ", pubkey)) %>%
						pull(alias_pubkey)
					if (channel_actions()[x] == "add") {
						isolate(targetUpdateServer('target_select', x, available_choices[!available_choices %in% peer_alias_pubkeys], targets()[x]))
					} else {
						isolate(targetUpdateServer('target_select', x, available_choices, targets()[x]))
					}
				}
			)
		})
		# reset centralities valueboxes to current values on subject/target
		# selection
		observeEvent(c(subject(), targets()), {
			centralities <- lapply(
				c('between', 'close', 'eigen'),
				function(x)
					centralityRankServer('cents', undir_graph, subject, x)
				)
			lapply(centralities, function(x) centralityRankOutput('cents', x))
		})

		dropdownFilterSelectServer('filters')
		resetFiltersServer('filters', subject)
		subjectSelectServer('subject_select')
		toggleHiddenElements("filters", credentials)

		# if the launch button is clicked, then run a simulation on the subject
		# with the given targets
		sim_run <- eventReactive(startButtonServer('launch_sim', buttonId="launch_sim_button"), {
			req(subject() != "")
			req(length(unique(targets())) != 1 && unique(targets()) != "")
			showModal(
				modalDialog(title="Running simulation, please wait...", withSpinner(uiOutput("loading"), size=2), size='s', footer=NULL)
			)
			channelSimulationServer('launch_sim', subject, targets, channel_actions, api_info)
		})

		# if a previous run has been completed, and a new target is selected,
		# then push this value to the client side to display previous results
		output$previous_results <- eventReactive(targets(), {
			req(sim_result())
			1
		})
		outputOptions(output, "previous_results", suspendWhenHidden=FALSE)

		upgradeButtonServer("ad_upgrade",
			p(HTML("Want us to find nodes that increase your centralities the most?<br/>Sign up!"), onclick="openTab('account')"))
		output$account_is_premium <- eventReactive(credentials(), {
			if (credentials()$premium) {
				return("true")
			} else {
				return("false")
			}
		})
		outputOptions(output, "account_is_premium", suspendWhenHidden=FALSE)

		# accumulate previous results
		observeEvent(targets(), {
			req(sim_result())
			formatUserChoices("past_action", previous_targets(), channel_actions)
			simulated_centralities <- lapply(
				c('between', 'close', 'eigen'),
				function(x)
					centralityRankServer('past_result', undir_graph, subject, x, sim_result())
				)
			lapply(simulated_centralities, function(x) centralityRankOutput('past_result', x))
		})

		# ping the background process and fetch results when done
		sim_result <- reactiveVal()
		observe({
			req(sim_run())
			if (isolate(sim_run()$is_alive())) {
				invalidateLater(1000)
			} else {
				sim_result(sim_run()$get_result())
				removeModal()
			}
		})

		# update centrality valueboxes with simulation results
		observeEvent(sim_result(), {
			req(!is.null(sim_result()))
			previous_targets(targets())
			simulated_centralities <- lapply(
				c('between', 'close', 'eigen'),
				function(x)
					centralityRankServer('cents', undir_graph, subject, x, sim_result())
				)
			lapply(simulated_centralities, function(x) centralityRankOutput('cents', x))
		})
		
		# filter by LN+ swaps
		filter_by_lnplus_swaps <- lnplusFilterActivate("filters")
		output$filter_by_lnplus_swaps <- eventReactive(filter_by_lnplus_swaps(), {
			filter_by_lnplus_swaps()
		})
		# pass the filter state to the client side to determine whether to show
		# swaps UI element
		outputOptions(output, "filter_by_lnplus_swaps", suspendWhenHidden=FALSE)
		# fetch qualifying swaps for the subject pubkey
		qualifying_swaps <- eventReactive(targets(), {
			req(filter_by_lnplus_swaps() == 1)
			pending_swaps %>% filter(pubkey %in% !!targets())
		})
		# update the swaps UI element depending on selected targets
		observeEvent(targets(), {
			req(filter_by_lnplus_swaps() == 1)
			output$show_qualifying_swaps <- lnplusSwapUIServer("show_qualifying_swaps", qualifying_swaps())
		})
	})
}

#' channel simulation app standalone
#'
#' for dev/testing purposes
chansimApp <- function() {
	chansim_api_info <- if (Sys.getenv("LOCAL")) {
			list(url=Sys.getenv("CHANSIM_LOCAL_API_URL"))
		} else {
			get_api_info("chansim-api")
		}
	ui <- dashboardPage(
		dashboardHeader(title='Channel Simulator'),
		dashboardSidebar(),
		dashboardBody(chansimUI('x')),
		skin='yellow',
	)
	credentials <- reactiveValues(
		info=data.frame(pubkey=test_pubkey, foo="bar"),
		user_auth=TRUE, premium=TRUE)
		#info=NULL,
		#user_auth=FALSE, premium=FALSE)
	server <- function(input, output, session) {
		chansimServer('x', chansim_api_info, reactive(reactiveValuesToList(credentials)))
	}
	shinyApp(ui, server)
}
