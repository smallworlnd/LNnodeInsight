source("inst/shiny-common.R", local=TRUE)

report_filters <- data.frame(
	filter_vars=c("max.cap", "max.med.capacity", "max.fee.rate", "max.num.channels", "max.between", "max.close", "max.eigen", "max.hops"),
	filter_max=tbl(pool, 'nodes_current') %>%
		summarise(
			max.cap=round(max(tot.capacity)/1e8+1, 0),
			max.med.capacity=max(med.capacity)/1e8,
			max.fee.rate=6000,
			max.num.channels=max(num.channels)+1,
			max.between=max(cent.between.rank),
			max.close=max(cent.close.rank),
			max.eigen=max(cent.eigen.rank),
			max.hops=11) %>%
		as_tibble %>%
		unlist(use.names=FALSE),
	filter_descr=c(
		'Filter by range of total capacity (in BTC)',
		'Filter by range of median channel capacity (in BTC)',
		'Filter by range of median channel fee rates (ppm)',
		'Filter by range of total channels',
		'Filter by range of betweenness centrality ranks',
		'Filter by range of closeness centrality ranks',
		'Filter by range of eigenvector centrality ranks',
		'Search nodes that fall within a range of hops away from your node'),
	filter_min=c(0.1, 0.005, 0, 5, 1, 1, 1, 1),
	filter_steps=c(0.1, 0.01, 1, 1, 1, 1, 1, 1)
	) %>% t %>% as.data.frame

#' infobox UI element
#'
#' displays info about account upgrade
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return returns infobox output containing subscription information
#' @export
subInfoBox <- function(id) {
	infoBoxOutput(NS(id, "sub_info"), width=NULL)
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

#' main layout UI for various report elements
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return return the main layout UI element
#' @export
reportsUI <- function(id) {
	ns <- NS(id)
	fluidRow(
		useShinyjs(),
		column(12,
			conditionalPanel(
				condition="output.account_is_premium == 'true'", ns=ns,
				h3('Automated weekly channel simulation optimization report', align="center"),
				column(8, offset=2,
					box(id=NS(id, "filt.box"), title=uiOutput(NS(id, 'targets_num')),
						width=NULL, collapsible=TRUE,
						collapsed=TRUE, solidHeader=TRUE, status='primary',
						lapply(
							report_filters,
							function(x)
								numRangeFilterSelectUI(
									id=NS(id, 'filters'),
									filtId=x[1], lab=x[3],
									minVal=as.numeric(x[4]), maxVal=as.numeric(x[2]),
									stepVal=as.numeric(x[5]))
						),
						h6("* Committed node variable filters will be activated the next time the optimization engine runs"),
						h6("* Can be updated at any time but new submissions override previous ones"),
						column(12, align="center",
							startButtonUI(NS(id, "commit_minmax_filters"),
								buttonId="update_minmax_filters",
								lab="Save search filters"),
							startButtonUI(NS(id, "reset_minmax_filters"),
								buttonId="reset_minmax_filters",
								lab="Reset filters to default",
								button_color="danger"),
						),
						column(12, align="center",
							shinyjs::hidden(
							  shiny::div(
								id=NS(id, "saved_filters"),
								shiny::tags$p(
								  "Filter settings saved!",
								  style="color: orange; font-weight: bold; padding-top: 5px;",
								  class="text-center"
								)
							  )
							),
						)
					),
				),
				column(12,
					dataTableUI(NS(id, "account_minmax_report"), "minmax"),
					style="height:500px; overflow-y: scroll;overflow-x: scroll;"
				),
				br(),
				column(12, h3('LightningNetwork+ swap optimization report', align="center")),
				column(12,
					dataTableUI(NS(id, "lnplus_minmax_report"), "lnplus_minmax"),
					style="overflow-x: scroll;"
				),
				column(12, align="center",
					startButtonUI(NS(id, "get_swap_minmax_report"),
						buttonId="start_lnplus_minmax",
						lab=swapRefreshButtonUI(NS(id, "swap_minmax_label")))
				),
			),
		),
		column(12, offset=2,
			conditionalPanel(
				condition="output.account_is_auth == 'false' || output.account_is_premium == 'false'", ns=ns,
				box(title=NULL, background='yellow', width=8,
					column(10, subInfoBox(NS(id, "show_sub_info"))),
					br(),
					column(2, align="center",
						upgradeButtonUI(NS(id, "ad_upgrade"))
					)
				)
			)
		)
	)
}

#' subcription infobox server
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns infobox render of subscription details
#' @export
subInfoServer <- function(id) {
	moduleServer(id, function(input, output, session) {
		output$sub_info <- renderInfoBox({
			infoBox(
				"", "Upgrade your account and see which nodes and LN+ swaps increase your centralities the most", icon=icon("exclamation"),
				color = "yellow", fill=TRUE
			)
		})
	})
}

swapRefreshButtonUI <- function(id) {
	textOutput(NS(id, "swap_minmax_button_label"))
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
			paste(length(targets_list), "nodes to be searched in the next optimization run")
		})
	})
}

swapRefreshButtonLabel <- function(id, credentials, lnplus_minmax_results) {
	moduleServer(id, function(input, output, session) {
		output$swap_minmax_button_label <- renderText({
			prev_res <- lnplus_minmax_results %>%
				filter(pubkey==!!credentials()$info[1]$pubkey) %>%
				collect %>%
				nrow
			if (prev_res > 0) {
				return("Refresh optimal swaps")
			} else {
				return("Find optimal swaps")
			}
		})
	})
}

#' lnplus swap optimization server
#'
#' module for computing optimal swaps to join based on centralities
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
lnplusSwapMinmax <- function(id, subject, api_info) {
	moduleServer(id, function(input, output, session) {
		# poorman's async request to chansim api by sending process to background
		# and avoid session locks
		sim_request <- callr::r_bg(
			func = function(subject_pubkey, api_info) {
				req_body <- jsonlite::toJSON(
					list(subject_pubkey=subject_pubkey), auto_unbox=TRUE)
				if ("token" %in% names(api_info)) {
					googleCloudRunner::cr_jwt_with_httr(
						httr::POST(url=api_info$url, body=req_body, encode="json"),
						api_info$token)
				} else {
					httr::POST(url=api_info$url, body=req_body, encode="json")
				}
				return(TRUE)
			},
			args=list(subject_pubkey=subject, api_info=api_info),
			supervise=TRUE
		)
		return(sim_request)
	})
}

#' minmax result server
#'
#' displays results obtained from centrality optimization
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials login status from \link{loginServer}
#' @param users users (sql) table containing account information
#' @return returns data table output server containing minmax results
#' @export
minmaxServer <- function(id, credentials, db=pool) {
	moduleServer(id, function(input, output, session) {
		output$minmax <- renderDataTable({
			req(credentials$user_auth)
			user_results_db <- tbl(db, "minmax") %>%
				filter(pubkey.x==!!pull(credentials$info[1])) %>%
				filter(time==max(time)) %>%
				left_join(., tbl(pool, "nodes_current"), by=c("pubkey.y"="pubkey")) %>%
				dplyr::select(c(time.x, alias.x, num.channels, tot.capacity,
					cent.between.rank.delta, cent.close.rank.delta, cent.eigen.rank.delta, pubkey.y)) %>%
				as_tibble %>%
				mutate(
					pubkey.y=paste0("<a href='https://lnnodeinsight.com/?/", pubkey.y, "' target='_blank'>", pubkey.y, "</a>"),
					tot.capacity=round(tot.capacity/1e8, 2)) %>%
				rename(c(
					"Run date"="time.x", "Target pubkey"="pubkey.y", "Target alias"="alias.x",
					"Number of channels"="num.channels", "Total capacity (BTC)"="tot.capacity",
					"Gain in betweenness rank"="cent.between.rank.delta",
					"Gain in closeness/hopness rank"="cent.close.rank.delta",
					"Gain in eigenvector/hubness rank"="cent.eigen.rank.delta"))
		}, escape=FALSE, options=list(autoWidth=TRUE, columnDefs=list(list(width='10px', targets=1))))
	})
}

#' lnplus minmax result server
#'
#' displays results obtained from lnplus swap optimization on centralities
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials login status from \link{loginServer}
#' @param users users (sql) table containing account information
#' @return returns data table output server containing minmax results
#' @export
lnplusMinmaxServer <- function(id, credentials, users, lnplus_minmax_results) {
	moduleServer(id, function(input, output, session) {
		output$lnplus_minmax <- renderDataTable({
			req(credentials$user_auth)
			user_results <- lnplus_minmax_results %>%
				filter(pubkey==!!pull(credentials$info[1])) %>%
				filter(time==max(time)) %>%
				dplyr::select(-pubkey) %>%
				as_tibble %>%
				mutate(
					top_swaps=paste0("<a href='https://lightningnetwork.plus/swaps/", top_swaps, "' target='_blank'>", top_swaps, "</a>"),
					swap_amt=prettyNum(swap_amt, big.mark=",")) %>%
				rename(c(
					"Run date"="time", "Swap ID"="top_swaps", "Swap amount"="swap_amt",
					"Currently enrolled"="num_participants", "Maximum participants"="participant_max_count",
					"Gain in betweenness rank"="cent.between.rank.delta",
					"Gain in closeness/hopness rank"="cent.close.rank.delta",
					"Gain in eigenvector/hubness rank"="cent.eigen.rank.delta"))
		}, escape=FALSE)
	})
}

getFilterInput <- function(id) {
	moduleServer(id, function(input, output, session) {
		return(
			reactive(
				lapply(
					report_filters[1, ],
					function(x) eval(parse(text=paste0("input$", x))))
			)
		)
	})
}

resetFilters <- function(id) {
	moduleServer(id, function(input, output, session) {
		lapply(report_filters, function(x)
			updateNumericRangeInput(session, inputId=x[1], value=as.numeric(c(x[4], x[2]))))
	})
}

getPredefinedFilters <- function(id, user_pubkey, db=pool) {
	moduleServer(id, function(input, output, session) {
		filters <- tbl(db, 'minmax_filters') %>%
			filter(pubkey==!!user_pubkey) %>%
			filter(time==max(time)) %>%
			as_tibble
		if (nrow(filters) > 0) {
			filters_reformat <- tbl(pool, 'minmax_filters') %>%
				filter(pubkey==!!user_pubkey) %>%
				filter(time==max(time)) %>%
				dplyr::select(min.cap:max.hops) %>%
				collect %>% matrix(ncol=2, byrow=TRUE) %>% t %>%
				rbind(report_filters[1, ], .)
			lapply(filters_reformat, function(x)
				updateNumericRangeInput(session, inputId=x[1], value=as.numeric(c(x[2], x[3]))))
		} else {
			lapply(report_filters, function(x)
				updateNumericRangeInput(session, inputId=x[1], value=as.numeric(c(x[4], x[2]))))
		}
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
applyInputFiltersServer <- function(id, graph=undir_graph, credentials, node_list=node_ids, db=pool) {
	moduleServer(id, function(input, output, session) {
		vals <- eventReactive(c(input$max.cap, input$max.med.capacity, input$max.fee.rate, input$max.num.channels, input$max.between, input$max.close, input$max.eigen, input$max.hops), {
			req(credentials$user_auth)
			# apply user-defined filters
			vals <- make_ego_graph(graph, order=input$max.hops[2]+1, nodes=fetch_id(pubkey=credentials$info[1]$pubkey), mindist=input$max.hops[1]+1)[[1]] %>%
				as_tbl_graph %>%
				filter(
					act.channels>0,
					act.channels/(act.channels+inact.channels)>0.66,
					tot.capacity>=input$max.cap[1]*1e8, tot.capacity<=input$max.cap[2]*1e8,
					!is.na(mean.rate.ppm),
					med.capacity>=input$max.med.capacity[1]*1e8, med.capacity<=input$max.med.capacity[2]*1e8,
					num.channels>=input$max.num.channels[1], num.channels<=input$max.num.channels[2],
					median.rate.ppm>=input$max.fee.rate[1], median.rate.ppm<=input$max.fee.rate[2],
					cent.between.rank>=input$max.between[1], cent.between.rank<=input$max.between[2],
					cent.close.rank>=input$max.close[1], cent.close.rank<=input$max.close[2],
					cent.eigen.rank>=input$max.eigen[1], cent.eigen.rank<=input$max.eigen[2]) %>%
				mutate(target=paste(alias, "-", pubkey))
			pull(vals, target)
		}, ignoreInit=TRUE)
		return(vals)
	})
}

#' main reports page server
#'
#' backend handling of all account-related information
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials login status from \link{loginServer}
#' @export
reportServer <- function(id, credentials, api_info, db=pool) {
	moduleServer(id, function(input, output, session) {
		users <- pool %>% tbl("users")
		lnplus_minmax_results <- pool %>% tbl("lnplus_minmax")
		minmaxServer("account_minmax_report", credentials())
		lnplusMinmaxServer("lnplus_minmax_report", credentials(), users, lnplus_minmax_results)
		upgradeButtonServer("ad_upgrade", p(HTML("Upgrade"), onclick="openTab('account')", style="text-align: center; height: 16px;"))
		subInfoServer("show_sub_info")
		output$account_is_premium <- premiumAccountReactive("prem_account", credentials, users)
		output$account_is_auth <- reactive({
			if (credentials()$user_auth) {
				return("true")
			} else {
				return("false")
			}
		})
		outputOptions(output, "account_is_premium", suspendWhenHidden=FALSE)
		outputOptions(output, "account_is_auth", suspendWhenHidden=FALSE)

		# set up ln+ swap minmax button
		lnplus_minmax_button <- startButtonServer("get_swap_minmax_report", buttonId="start_lnplus_minmax")
		# on button press, compute latest lnplus swap minmax
		lnplus_minmax_run <- eventReactive(lnplus_minmax_button(), {
			showModal(
				modalDialog(
					title="Started LN+ swap recommender",
					"Running simulations, should take several minutes. The page will refresh when results are in.",
					footer=NULL
				)
			)
			lnplusSwapMinmax("get_swap_minmax", credentials()$info[1]$pubkey, api_info)
		})
		lnplus_minmax_result <- reactiveVal()
		# wait for result to come in
		observe({
			req(lnplus_minmax_run())
			if (isolate(lnplus_minmax_run()$is_alive())) {
				invalidateLater(2000)
			} else {
				isolate(lnplus_minmax_result(TRUE))
				removeModal()
				refresh()
			}
		})
		swapRefreshButtonLabel("swap_minmax_label", credentials, lnplus_minmax_results)
		
		observe({
			req(credentials()$info[1]$pubkey != "")
			isolate(getPredefinedFilters("filters", user_pubkey=credentials()$info[1]$pubkey))
		})
		userChangeFilters <- getFilterInput("filters")
		filterOutput <- applyInputFiltersServer("filters", credentials=credentials())
		output$targets_num <- renderUI({paste("Optional: expand this bar to apply node filters at the time the optimization engine runs*")})
		observeEvent(filterOutput(), {
			output$targets_num <- renderFilterBarServer('filterbar', filterOutput())
		})
		node_filters_save <- startButtonServer("commit_minmax_filters", buttonId="update_minmax_filters")
		node_filters_reset <- startButtonServer("reset_minmax_filters", buttonId="reset_minmax_filters")
		observeEvent(node_filters_save(), {
			filts <- userChangeFilters() %>% unlist %>% as.data.frame %>% t %>% as.data.frame
			names(filts) <- c("min.cap", "max.cap", "min.med.capacity", "max.med.capacity",
				"min.fee.rate", "max.fee.rate", "min.num.channels", "max.num.channels",
				"min.between", "max.between", "min.close", "max.close", "min.eigen", "max.eigen", "min.hops", "max.hops")
			filts$time <- now("GMT")
			filts$pubkey <- credentials()$info[1]$pubkey
			dbWriteTable(pool, "minmax_filters", filts, row.names=FALSE, append=TRUE)
			shinyjs::toggle(id="saved_filters", anim=TRUE, time=1, animType="fade")
			shinyjs::delay(500, shinyjs::toggle(id="saved_filters", anim=TRUE, time=3, animType="fade"))
		})
		observeEvent(node_filters_reset(), {
			resetFilters("filters")
		})
	})
}

#' reports page standalone
#'
#' for dev/testing purposes
reportsApp <- function() {
	lnplus_minmax_api_info <- if (Sys.getenv("LOCAL")) {
			list(url=Sys.getenv("LNPLUS_MINMAX_LOCAL_API_URL"))
		} else {
			get_api_info("lnplus-swap-minmax")
		}
	ui <- dashboardPage(
		dashboardHeader(title='Reports Page'),
		dashboardSidebar(),
		dashboardBody(reportsUI('x')),
		skin='yellow',
	)
	credentials <- reactiveValues(
		info=data.frame(pubkey=test_pubkey, foo="bar"),
		user_auth=TRUE)
	server <- function(input, output, session) {
		reportServer('x', reactive(credentials), lnplus_minmax_api_info)
	}
	shinyApp(ui, server)
  
}
