source("inst/shiny-common.R", local=TRUE)

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

#' main reports page server
#'
#' backend handling of all account-related information
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials login status from \link{loginServer}
#' @export
reportServer <- function(id, credentials, api_info) {
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
