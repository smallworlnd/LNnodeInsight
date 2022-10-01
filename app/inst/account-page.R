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

#' infobox UI element
#'
#' displays info about account upgrade
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return returns infobox output containing subscription information
#' @export
subInfoBox <- function(id) {
	infoBoxOutput(NS(id, "sub_info"), width=4)
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

#' subcription infobox server
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns infobox render of subscription details
#' @export
subInfoServer <- function(id) {
	moduleServer(id, function(input, output, session) {
		output$sub_info <- renderInfoBox({
			infoBox(
				"", "Upgrade to get weekly centrality optimization reports for your node, on-demand LN+ swap recommendations, and get unlimited access to all LNnodeInsight tools", icon=icon("exclamation"),
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
accountServer <- function(id, credentials) {
	moduleServer(id, function(input, output, session) {
		users <- pool %>% tbl("users")
		accountHeaderServer("account_header", credentials(), users)
		accountTableServer("account_status", credentials(), users)
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
				existing_entry <- users %>%
					filter(pubkey %in% !!new_sub_df$pubkey, subscription %in% !!new_sub_df$subscription) %>%
					as_tibble %>%
					filter(sub_date==new_sub_df$sub_date, sub_expiration_date==new_sub_df$sub_expiration_date)
				if (nrow(existing_entry) > 0) {
					return(TRUE)
				} else {
					dbWriteTable(pool, "users", new_sub_df, row.names=FALSE, append=TRUE, overwrite=FALSE)
				}},
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
						<li>The first centrality optimization report should appear on the Reports page within 24 hours</li>
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
		info=data.frame(pubkey=test_pubkey, foo="bar"),
		user_auth=TRUE)
	server <- function(input, output, session) {
		accountServer('x', reactive(credentials))
	}
	shinyApp(ui, server)
}
