source("inst/shiny-common.R")

#' global variable useful for this app only
chansim_filters <- data.frame(
	filter_vars=c("max.cap", "max.avg.capacity", "max.fee.rate", "max.num.channels", "max.between", "max.close", "max.eigen", "max.hops"),
	filter_max=tbl(pool, 'nodes_current') %>%
		summarise(
			max.cap=round(max(tot.capacity)/1e8, 0),
			max.avg.capacity=max(avg.capacity)/1e8,
			max.fee.rate=6000,
			max.num.channels=max(num.channels)+1,
			max.between=max(cent.between.rank),
			max.close=max(cent.close.rank),
			max.eigen=max(cent.eigen.rank),
			max.hops=11) %>%
		as_tibble %>%
		unlist(use.names=FALSE),
	filter_descr=c(
		'Filter by range of total capacity (in bitcoin)',
		'Filter by range of average channel capacity (in bitcoin)',
		'Filter by range of average channel fee rates (ppm)',
		'Filter by range of total channels',
		'Filter by range of betweenness centrality ranks',
		'Filter by range of closeness centrality ranks',
		'Filter by range of eigenvector centrality ranks',
		'Only show nodes that fall within a range of hops away from the node selected in Step 1'),
	filter_min=c(0.01, 0.001, 0, 1, 1, 1, 1, 0),
	filter_steps=c(0.1, 0.01, 1, 1, 1, 1, 1, 1)
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

#' UI slider element for the various node filters
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param filtId filter-specific short ID
#' @param lab descriptive label for the ui element
#' @param minVal minimum value for the filter
#' @param maxVal maximum value for the filter
#' @param stepVal slider step increments
#' @return return slider filt UI element
#' @export
sliderFilterSelectUI <- function(id, filtId, lab, minVal, maxVal, stepVal) {
	sliderInput(
		inputId=NS(id, filtId),
		label=lab,
		ticks=FALSE,
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

					lapply(
						chansim_filters,
						function(x)
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
					startButtonUI(NS(id, 'launch_sim'), lab="Start")
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
					peerVennUI(NS(id, 'chansim_venn')),
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
		),
		conditionalPanel(
			"output.previous_results == 1", ns=ns,
			column(4,
				fluidRow(
					box(id=NS(id, 'prev.cent.box'), title="Previous run results",
						solidHeader=TRUE, collapsible=TRUE, width=NULL,
						pastRunUI(NS(id, "past_action")),
						br(),
						centralityUI(NS(id, "past_result"))
					)
				)
			)
		)
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

#' pubkey filter resetting server
#'
#' module for resetting \link{sliderFilterSelectUI},
#' \link{dropdownFilterSelectUI}, \link{buttonFilterSelectUI}
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param subject reactively reset filters on subject pubkey change
#' @param filters data.frame of peer filters specified in global.R
#' @param db backend database for accessing amboss community list
#' @return reset filters
#' @export
resetFiltersServer <- function(id, subject, filters=chansim_filters, db=con) {
	moduleServer(id, function(input, output, session) {
		observeEvent(subject(), {
			lapply(chansim_filters, function(x)
				updateSliderInput(session, inputId=x[1], value=c(x[4], x[2]))
			)
			updateSliderInput(session, inputId='max.hops', value=c(0, 11))
			updateSliderInput(session, inputId='peers.of.peers', value=2)

			comms <- tbl(pool, 'communities') %>%
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
#' @param sim_output reactive graph from a simulation run which includes the
#' modified centralities
#' @return return centrality ranks, either current or simulated
#' @export
centralityRankServer <- function(id, graph=undir_graph, subject, metric, sim_output=NULL) {
	moduleServer(id, function(input, output, session) {
		result_metric <- paste0('cent.', metric, '.rank')
		label <- ifelse(metric == "between", "Betweenness",
			ifelse(metric == "close", "Closeness/hopness",
			"Eigenvector/hubness"))
		if (!is.null(sim_output)) {
			data <- sim_output() %>%
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
peerVennServer <- function(id, subject, targets) {
	moduleServer(id, function(input, output, session) {
		output$chansim_venn <- renderPlotly({
			req(subject() != "")
			req(length(targets()[targets() != ""]) > 0)
			xkey <- subject()
			xkey_alias <- fetch_alias(pubkey=xkey)
			ykeys <- targets()[targets() != ""]
			ykey_aliases <- sapply(ykeys, function(z) fetch_alias(pubkey=z)) %>% as.vector
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
		targets <- targets()[targets() != ""]
		indels <- add_or_del()[which(targets() != '')]

		showModal(modalDialog("Running simulation, please wait...", size='s', footer=NULL))
		# build request to chansim api
		sim_req_body <- toJSON(
			list(
				subject_pubkey=subject(),
				target_pubkeys=targets,
				indel=indels
			),
			auto_unbox=TRUE)
		if ("token" %in% names(api_info)) {
			sim_query <- cr_jwt_with_httr(
				POST(
					url=api_info$url,
					body=sim_req_body,
					encode="json"),
				api_info$token)
		} else {
			sim_query <- POST(
				url=api_info$url,
				body=sim_req_body,
				encode="json")
		}
		sim_resp <- content(sim_query)
		removeModal()

		return(reactive(sim_resp))
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
applyFiltersToTargetsServer <- function(id, graph=undir_graph, pubkey, node_list=node_ids, db=con) {
	moduleServer(id, function(input, output, session) {
		if (pubkey != "") {
			vals <- make_ego_graph(graph, order=input$max.hops[2]+1, nodes=fetch_id(pubkey=pubkey), mindist=input$max.hops[1]+1)[[1]] %>%
				as_tbl_graph %>%
				filter(
					tot.capacity>=input$max.cap[1]*1e8, tot.capacity<=input$max.cap[2]*1e8,
					avg.capacity>=input$max.avg.capacity[1]*1e8, avg.capacity<=input$max.avg.capacity[2]*1e8,
					num.channels>=input$max.num.channels[1], num.channels<=input$max.num.channels[2],
					median.rate.ppm>=input$max.fee.rate[1], median.rate.ppm<=input$max.fee.rate[2],
					cent.between.rank>=input$max.between[1], cent.between.rank<=input$max.between[2],
					cent.close.rank>=input$max.close[1], cent.close.rank<=input$max.close[2],
					cent.eigen.rank>=input$max.eigen[1], cent.eigen.rank<=input$max.eigen[2]) %>%
				mutate(target=paste(alias, "-", pubkey))
				if (input$community != "") {
					members <- tbl(pool, 'communities') %>%
						filter(community==local(input$community)) %>%
						pull(pubkey)
					vals <- vals %>% filter(pubkey %in% members)
				}
			if (as.numeric(input$peers.of.peers) == 1) {
				peers_of_peers <- fetch_peers_of_peers(pubkey=pubkey) %>% unlist %>% unique
				vals <- vals %>% as_tibble %>% filter(!(alias %in% peers_of_peers)) %>% pull(target)
			} else {
				vals <- vals %>% as_tibble %>% pull(target)
			}
			return(vals)
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

#' dropdown filter selection server
#'
#' module for dropdown menu filters, like for filtering by amboss community
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param db db from which we pull community information (maybe should be a
#' vector instead)
#' @param logged_in make community filter accessible if user is logged in
#' @return returns list of amboss communities from which to filter target node
#' pubkeys
#' @export
dropdownFilterSelectServer <- function(id, db=pool, logged_in=FALSE) {
	moduleServer(id, function(input, output, session) {
		shiny::observe({
			shinyjs::toggle('community', condition=logged_in())
		})
		comms <- tbl(pool, 'communities') %>% pull(community) %>% unique %>% sort
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
formatUserChoices <- function(id, targets, actions) {
	moduleServer(id, function(input, output, session) {
		output$target_actions <- renderText({
			aliases <- sapply(targets[targets != ""], function(x) fetch_alias(pubkey=x))
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
#' @param reactive_show reactively hide/show some filters depending on user
#' account
#' @param api_info api url and auth token to POST channel simulation information
#' to local (if no auth token present) or remote backend
#' @export
chansimServer <- function(id, reactive_show, api_info) {
	moduleServer(id, function(input, output, session) {
		# initializ reactive values
		subject <- getNodePubkey('subject_select', "subject")
		targets <- getTargets('target_select')
		available_choices <- reactiveVal()
		sim_output <- reactiveValues()
		channel_actions <- getChannelActions('target_select')
		launch_sim <- startButtonServer('launch_sim', buttonId="launch_sim_button")
		output$chansim_venn <- peerVennServer('chansim_venn', subject, targets)
		previous_targets <- reactiveVal(NULL)

		# adapt target lists on observing any filter changes
		observe({
			available_choices <- applyFiltersToTargetsServer('filters', pubkey=subject())
			output$targets_num <- renderFilterBarServer('filterbar', available_choices)
			isolate(targetUpdateServer('target_select', available_choices))
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

		dropdownFilterSelectServer('filters', db=pool, logged_in=reactive_show)
		resetFiltersServer('filters', subject)
		subjectSelectServer('subject_select')

		# if the launch button is clicked, then run a simulation on the subject
		# with the given targets
		sim_output <- eventReactive(launch_sim(), {
			req(subject() != "")
			req(length(unique(targets())) != 1 && unique(targets()) != "")
			channelSimulationServer('launch_sim', subject, targets, channel_actions, api_info)
		})

		# if a previous run has been completed, and a new target is selected,
		# then push this value to the client side to display previous results
		output$previous_results <- eventReactive(targets(), {
			req(sim_output())
			1
		})
		outputOptions(output, "previous_results", suspendWhenHidden=FALSE)

		# accumulate previous results
		observeEvent(targets(), {
			req(sim_output())
			formatUserChoices("past_action", previous_targets(), channel_actions)
			simulated_centralities <- lapply(
				c('between', 'close', 'eigen'),
				function(x)
					centralityRankServer('past_result', undir_graph, subject, x, sim_output())
				)
			lapply(simulated_centralities, function(x) centralityRankOutput('past_result', x))
		})

		# update centrality valueboxes with simulation results
		observeEvent(sim_output(), {
			previous_targets(targets())
			simulated_centralities <- lapply(
				c('between', 'close', 'eigen'),
				function(x)
					centralityRankServer('cents', undir_graph, subject, x, sim_output())
				)
			lapply(simulated_centralities, function(x) centralityRankOutput('cents', x))
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
	server <- function(input, output, session) {
		chansimServer('x', reactive_show=reactive(FALSE), chansim_api_info)
	}
	shinyApp(ui, server)
}
