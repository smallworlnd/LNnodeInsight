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
accountTableUI <- function(id, tableId) {
	tableOutput(NS(id, tableId))
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
				fluidRow(
					column(12,
						box(title=NULL, background='yellow', width=NULL,
							accountHeaderUI(NS(id, "account_header")),
							accountTableUI(NS(id, "account_status"), "account"),
							conditionalPanel(
								condition="output.account_is_premium == 'false'", ns=ns,
								br(),
								column(12, align="center",
									startButtonUI(NS(id, "subscribe_button"), buttonId="action_button", lab="Upgrade account")
								),
							)
						),
					)
				),
				fluidRow(
					column(12,
						box(title=NULL, background='yellow', width=NULL,
							h4(strong("API access")),
							accountTableUI(NS(id, "account_keys"), "keys"),
							column(12, align="center",
								startButtonUI(NS(id, "generate_key_button"), buttonId="key_button", lab="Generate API key")
							),
						),
					)
				)
			)
		)
	)
}

#' account table information server
#'
#' summarises account information like account type, subscription dates and
#' expiration dates
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials login status from \link{loginServer}
#' @param db sql db
#' @return returns renderTable server for account information
#' @export
accountTableServer <- function(id, credentials, db=pool) {
	moduleServer(id, function(input, output, session) {
		output$account <- renderTable({
			req(credentials$user_auth)
			acc <- tbl(db, "users") %>%
				filter(pubkey==!!pull(credentials$info[1])) %>%
				as_tibble %>%
				mutate(sub_date=format(sub_date, "%B %d, %Y"), sub_expiration_date=format(sub_expiration_date, "%B %d, %Y")) %>%
				dplyr::select(subscription, sub_date, sub_expiration_date, paymentHash) %>%
				rename(c(
					"Subscription"="subscription",
					"Subscription date"="sub_date",
					"Subscription expiration date"="sub_expiration_date",
					"Payment hash"="paymentHash"))
		}, spacing="l", width="100%", align="c")
	})
}

#' account box header
#'
#' displays node alias as header
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials login status from \link{loginServer}
#' @param db sql db
#' @return returns text UI element containing node alias
#' @export
accountHeaderServer <- function(id, credentials, db=pool) {
	moduleServer(id, function(input, output, session) {
		output$header <- renderUI({
			req(credentials$user_auth)
			node_alias <- tbl(db, "nodes_current") %>% filter(pubkey==!!pull(credentials$info[1])) %>% pull(alias)
			p(style="text-align: left; font-size: 20px", strong(node_alias))
		})
	})
}

#' account api key information server
#'
#' summarises api key information related to the account
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials login status from \link{loginServer}
#' @param db sql db
#' @return returns renderTable server for account information
#' @export
apiKeyTableServer <- function(id, credentials, db=pool) {
	moduleServer(id, function(input, output, session) {
		output$keys <- renderTable({
			req(credentials$user_auth)
			tbl(pool, "api_keys") %>%
				filter(pubkey==!!pull(credentials$info[1])) %>%
				dplyr::select(api_key, issue_date, expiry_date) %>%
				collect %>%
				mutate(api_key=paste0(substr(api_key, 1, 10), "......", substr(api_key, nchar(api_key)-10, nchar(api_key)))) %>%
				mutate_at(vars(issue_date, expiry_date), ~format(as_datetime(., tz="UTC"), "%B %d, %Y")) %>%
				dplyr::rename("API key"="api_key", "Date issued"="issue_date", "Expiration date"="expiry_date")
		}, spacing="l", width="100%", align="c")
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
accountServer <- function(id, credentials, db=pool) {
	moduleServer(id, function(input, output, session) {
		accountHeaderServer("account_header", credentials(), db=pool)
		accountTableServer("account_status", credentials(), db=pool)
		apiKeyTableServer("account_keys", credentials(), db=pool)
		subInfoServer("show_sub_info")
		output$account_is_premium <- eventReactive(credentials(), {
			if (credentials()$premium) {
				return("true")
			} else {
				return("false")
			}
		})
		output$account_is_auth <- eventReactive(credentials(), {
			if (credentials()$user_auth) {
				return("true")
			} else {
				return("false")
			}
		})
		outputOptions(output, "account_is_premium", suspendWhenHidden=FALSE)
		outputOptions(output, "account_is_auth", suspendWhenHidden=FALSE)

		observeEvent(startButtonServer("subscribe_button", buttonId="action_button"), {
			ns <- session$ns
			showModal(
				modalDialog(
					"By upgrading, you get:",
					HTML(
						"<ul>
						<li>A weekly report on automated channel simulations to show you economically<br/>active nodes that improve your centralities the most (first report shows up within 24 hours)</li>
						<li>On-demand search for LightningNetwork+ swaps to join that increase your centralities the most</li>
						<li>A breakdown of your outbound liquidity value</li>
						<li>Unlimited access to the rebalance simulator</li>
						<li>Unlimited access to the past centrality ranks and Terminal Web ranks</li>
						<li>Earn more sats than the current market bid for some node data</li>
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
			req(invoice()$status == "Paid")
			node_pubkey <- credentials()$info[1]$pubkey
			node_alias <- tbl(db, "nodes_current") %>% filter(pubkey==node_pubkey) %>% pull(alias)
			sub_begin <- now()
			sub_end <- now()+months(sub_period())
			new_sub_df <- data.frame(
				pubkey=node_pubkey, alias=node_alias, subscription="Premium",
				sub_date=sub_begin, sub_expiration_date=sub_end, paymentHash=invoice()$paymentHash)
			dbWriteTable(pool, "users", new_sub_df, row.names=FALSE, append=TRUE, overwrite=FALSE)
		})
		# show message after invoice successfully paid
		observeEvent(add_sub_to_db(), {
			req(add_sub_to_db())
			ns <- session$ns
			showModal(
				modalDialog(
					HTML(paste("Invoice paid", icon("check"), "thanks for subscribing! What happens next?")),
					HTML(
						"<ul>
						<li>The first centrality optimization report should appear on the Reports page within 24 hours</li>
						<li>Centrality optimizations are updated each week starting from the subscription date</li>
						<li>The page will refresh to reflect your account upgrade</li>
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

		observeEvent(startButtonServer("generate_key_button", buttonId="key_button"), {
			ns <- session$ns
			new_key <- tibble(
				pubkey=credentials()$info[1]$pubkey,
				api_key=stri_rand_strings(1, 64, pattern = "[A-Za-z0-9]"),
				issue_date=as.numeric(now()),
				expiry_date=as.numeric(now()+months(6))
			)
			dbWriteTable(db, "api_keys", new_key, overwrite=FALSE, append=TRUE)
			showModal(
				modalDialog(
					rclipboardSetup(),
					title="New API key generated!",
					column(12, align="center", code(new_key$api_key)),
					br(), br(),
					column(12, align="right", em("Visible only once")),
					footer=tagList(
						rclipButton(ns("new_key_clipbtn"), "Copy", new_key$api_key, icon("clipboard"), modal=TRUE),
						modalActionButton(ns("new_key_done"), "Done")
					)
				)
			)
			apiKeyTableServer("account_keys", credentials(), db=pool)
		})
	})
}

#' account page standalone
#'
#' for dev/testing purposes
accountApp <- function() {
	ui <- dashboardPage(
		dashboardHeader(title='Account Page'),
		dashboardSidebar(),
		dashboardBody(accountUI('x')),
		skin='yellow',
	)
	credentials <- reactiveValues(
		#info=data.frame(pubkey="", foo=""),
		#user_auth=FALSE)
		info=data.frame(pubkey=test_pubkey, foo="bar"),
		user_auth=TRUE, premium=TRUE)
	server <- function(input, output, session) {
		accountServer('x', reactive(credentials))
	}
	shinyApp(ui, server)
}
