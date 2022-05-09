source("inst/shiny-common.R", local=TRUE)
source('inst/invoice-mgmt.R', local=TRUE)

#' account header UI element
#'
#' displays node alias
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return returns ui output element corresponding to the renderText server
#' @export
accountHeaderUI <- function(id) {
	uiOutput(NS(id, "header"))
}

#' account summary table UI
#'
#' displays information about current account status and previous status changes
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param tableId the ID string corresponding to the lower level module function
#' @return returns simple table output of previous account activity
#' @export
tableUI <- function(id, tableId) {
	tableOutput(NS(id, tableId))
}

#' data table UI element
#'
#' used for centrality minmax reports
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param tableId the ID string corresponding to the lower level module function
#' @return returns interactive data table output
#' @export
dataTableUI <- function(id, tableId) {
	dataTableOutput(NS(id, tableId))
}

#' infobox UI element
#'
#' displays info about account upgrade
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return returns infobox output containing subscription information
#' @export
subInfoBox <- function(id) {
	infoBoxOutput(NS(id, "sub_info"), width=12)
}

#' subscription period slider UI element
#'
#' takes user input for desired subscription period
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return returns slider input UI element
#' @export
subPeriodSliderUI <- function(id) {
	sliderInput(NS(id, "sub_time_slider"), label="", min=1, max=6, step=1, value=1, ticks=FALSE, width="100%")
}

#' main layout UI for various account page elements
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return return the main layout UI element
#' @export
accountUI <- function(id) {
	ns <- NS(id)
	fluidRow(
		useShinyjs(),
		column(8, offset=2,
			conditionalPanel(
				condition="output.account_is_auth == 'true'", ns=ns,
				box(title=NULL, background='yellow', width=12,
					accountHeaderUI(NS(id, "account_header")),
					tableUI(NS(id, "account_status"), "account"),
					conditionalPanel(
						condition="output.account_is_premium == 'false'", ns=ns,
						column(10, subInfoBox(NS(id, "show_sub_info"))),
						br(),
						column(2, align="center", startButtonUI(NS(id, "subscribe_button"), buttonId="action_button", lab="Upgrade"))
					)
				)
			)
		),
		column(12,
			conditionalPanel(
				condition="output.account_is_premium == 'true'", ns=ns,
				h3('Automated weekly channel simulation optimization report', align="center"),
				column(12,
					dataTableUI(NS(id, "account_minmax_report"), "minmax"),
					style="height:500px; overflow-y: scroll;overflow-x: scroll;"
				),
				h5('*Results can take up to 24 hours to display after successfully paying subscription invoice', align="left"),
				br(),
				h3('LightningNetwork+ swap optimization report', align="center"),
				column(12,
					dataTableUI(NS(id, "lnplus_minmax_report"), "lnplus_minmax"),
					style="overflow-x: scroll;"
				),
				column(12, align="center",
					startButtonUI(NS(id, "get_swap_minmax_report"),
						buttonId="start_lnplus_minmax",
						lab=swapRefreshButtonUI(NS(id, "swap_minmax_label")))
				),
			)
		)
	)
}

swapRefreshButtonUI <- function(id) {
	textOutput(NS(id, "swap_minmax_button_label"))
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

#' account table information server
#'
#' summarises account information like account type, subscription dates and
#' expiration dates
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials login status from \link{loginServer}
#' @param users users (sql) table containing account information
#' @return returns renderTable server for account information
#' @export
accountTableServer <- function(id, credentials, users) {
	moduleServer(id, function(input, output, session) {
		output$account <- renderTable({
			req(credentials$user_auth)
			acc <- users %>%
				filter(pubkey==!!pull(credentials$info[1])) %>%
				as_tibble %>%
				mutate(sub_date=format(sub_date, "%B %d, %Y"), sub_expiration_date=format(sub_expiration_date, "%B %d, %Y")) %>%
				dplyr::select(subscription, sub_date, sub_expiration_date) %>%
				rename(c("Subscription"="subscription", "Subscription date"="sub_date", "Subscription expiration date"="sub_expiration_date"))
		}, spacing="l", width="100%", align="c")
	})
}

#' account box header
#'
#' displays node alias as header
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials login status from \link{loginServer}
#' @param users users (sql) table containing account information
#' @return returns text UI element containing node alias
#' @export
accountHeaderServer <- function(id, credentials, users) {
	moduleServer(id, function(input, output, session) {
		output$header <- renderUI({
			req(credentials$user_auth)
			node_alias <- users %>% filter(pubkey==!!pull(credentials$info[1])) %>% filter(sub_date==max(sub_date)) %>% pull(alias) %>% unique
			p(style="text-align: left; font-size: 20px", strong(node_alias))
		})
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
minmaxServer <- function(id, credentials, users, minmax_results) {
	moduleServer(id, function(input, output, session) {
		output$minmax <- renderDataTable({
			req(credentials$user_auth)
			user_results <- minmax_results %>%
				filter(pubkey.x==!!pull(credentials$info[1])) %>%
				filter(time==max(time)) %>%
				dplyr::select(-pubkey.x) %>%
				as_tibble %>%
				rename(c(
					"Run date"="time", "Target pubkey"="pubkey.y", "Target alias"="alias",
					"Gain in betweenness rank"="cent.between.rank.delta",
					"Gain in closeness/hopness rank"="cent.close.rank.delta",
					"Gain in eigenvector/hubness rank"="cent.eigen.rank.delta"))
		})
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

#' subcription infobox server
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns infobox render of subscription details
#' @export
subInfoServer <- function(id) {
	moduleServer(id, function(input, output, session) {
		output$sub_info <- renderInfoBox({
			infoBox(
				"", "Upgrade to get weekly centrality optimization reports for your node and get unlimited access to all LNnodeInsight tools", icon=icon("exclamation"),
				color = "yellow", fill=TRUE
			)
		})
	})
}

#' subcription period reactive server
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns reactive of subscription period slider
#' @export
subPeriodServer <- function(id) {
	moduleServer(id, function(input, output, session) {
		reactive(input$sub_time_slider)
	})
}

#' subcription amount reactive server
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns reactive of subscription amount based on period selected
#' @export
subPeriodAmountServer <- function(id) {
	moduleServer(id, function(input, output, session) {
		reactive(
			as.character(as.numeric(Sys.getenv("SUB_MSAT"))*input$sub_time_slider*(100-input$sub_time_slider+1)/100)
		)
	})
}

#' subcription period+amount reactive server
#'
#' generates reactive information on subscription period + amount to show in modal dialog
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param amt subscription amount (in msat)
#' @return returns reactive of subscription amount based on period selected
#' @export
subPeriodDisplayServer <- function(id, amt) {
	moduleServer(id, function(input, output, session) {
		output$sub_period_choice <- renderText({
			paste(
				"Move the slider to choose a subscription period:",
				input$sub_time_slider,
				ifelse(input$sub_time_slider==1, "month", "months"),
				paste0(" (", input$sub_time_slider-1, "%"), "discount)",
				"for", as.numeric(amt())/1e3, "sats")
		})
	})
}


#' main account page server
#'
#' backend handling of all account-related information
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials login status from \link{loginServer}
#' @export
accountServer <- function(id, credentials, api_info) {
	moduleServer(id, function(input, output, session) {
		users <- pool %>% tbl("users")
		minmax_results <- pool %>% tbl("minmax")
		lnplus_minmax_results <- pool %>% tbl("lnplus_minmax")
		accountHeaderServer("account_header", credentials(), users)
		accountTableServer("account_status", credentials(), users)
		minmaxServer("account_minmax_report", credentials(), users, minmax_results)
		lnplusMinmaxServer("lnplus_minmax_report", credentials(), users, lnplus_minmax_results)
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

		sub_button <- startButtonServer("subscribe_button", buttonId="action_button")
		observeEvent(sub_button(), {
			ns <- session$ns
			showModal(
				modalDialog(
					"By upgrading, you get:",
					HTML(
						"<ul>
						<li>A weekly breakdown of automated channel simulations to show you economically<br/>active nodes that improve your centralities the most (first report shows up within 24 hours)</li>
						<li>Unlimited access to the rebalance simulator</li>
						<li>Unlimited access to the past centrality ranks and Terminal Web ranks</li>
						<li>Automatic access to new features</li>
						<li>To support a Bitcoin Lightning project!</li>
						</ul>"),
					subPeriodDisplayServer("sub_period_choice", sub_period_amt),
					subPeriodSliderUI(NS(id, "sub_period_choice")),
					footer=tagList(
						modalActionButton(ns("cancel"), "Cancel"),
						modalActionButton(ns("create_invoice"), "Create invoice"),
					)
				)
			)
		})
		sub_period <- subPeriodServer("sub_period_choice")
		sub_period_amt <- subPeriodAmountServer("sub_period_choice")
		create_inv <- reactive(input$create_invoice)
		# generate an invoice on button click
		invoice <- invoiceHandlingServer(
			"subscription",
			reactive_trigger=create_inv,
			inv_fetch_url=Sys.getenv("STORE_URL"),
			inv_amt=sub_period_amt(),
			display_desc=HTML(paste(
				"Please pay this invoice for", as.numeric(sub_period_amt())/1e3,
				"sats<br/>to unlock premium features for", sub_period(), "months")),
			inv_desc=paste("subscription", credentials()$info[1], "for", sub_period(), "months")) %>% debounce(1000)

		# if invoice gets paid then modify subscription status in users db
		add_sub_to_db <- eventReactive(invoice(), {
			req(invoice() == "Paid")
			node_pubkey <- credentials()$info[1]$pubkey
			node_alias <- users %>% filter(pubkey==node_pubkey) %>% pull(alias)
			sub_begin <- now()
			sub_end <- now()+months(sub_period())
			new_sub_df <- data.frame(
				pubkey=node_pubkey, alias=node_alias, subscription="Premium",
				sub_date=sub_begin, sub_expiration_date=sub_end)
			tryCatch({
				dbWriteTable(pool, "users", new_sub_df, row.names=FALSE, append=TRUE, overwrite=FALSE)
				},
				error = function(e) {
					return(FALSE)
				}
			)
		})
		# show message after invoice successfully paid
		observeEvent(add_sub_to_db(), {
			req(add_sub_to_db())
			ns <- session$ns
			showModal(
				modalDialog(
					"Invoice paid, thanks for subscribing! What happens next?",
					HTML(
						"<ul>
						<li>The first centrality optimization report should appear on the account page within 24 hours</li>
						<li>That list will be updated each week</li>
						<li>The page will be refreshed after closing this box to reflect your account changes</li>
						</ul>"),
					footer=tagList(
						modalActionButton(ns("done"), "Done")
					)
				)
			)
		})
		# refresh page to reflect account changes
		observeEvent(input$done, {
			refresh()
		})
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
	})
}

#' account page standalone
#'
#' for dev/testing purposes
accountApp <- function() {
	lnplus_minmax_api_info <- if (Sys.getenv("LOCAL")) {
			list(url=Sys.getenv("LNPLUS_MINMAX_LOCAL_API_URL"))
		} else {
			get_api_info("lnplus-swap-minmax")
		}
	ui <- dashboardPage(
		dashboardHeader(title='Account Page'),
		dashboardSidebar(),
		dashboardBody(accountUI('x')),
		skin='yellow',
	)
	credentials <- reactiveValues(
		info=data.frame(pubkey=test_pubkey, foo="bar"),
		user_auth=TRUE)
	server <- function(input, output, session) {
		accountServer('x', reactive(credentials), lnplus_minmax_api_info)
	}
	shinyApp(ui, server)
  
}
