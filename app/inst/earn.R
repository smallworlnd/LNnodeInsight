source("inst/shiny-common.R", local=TRUE)

#' node select UI element
#'
#' dropdown/selectize list used to display and select from the list of nodes
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param listId list-specific ID that corresponds with the node list input
#' module, e.g., either for a subject or a target
#' @param lab label displayed in the middle of the button, e.g. "Start"
#' @return returns the selectize input UI element for node selection
#' @export
nodeDataSelectUI <- function(id) {
	selectInput(
		inputId=NS(id, "node_data"),
		label="Select data from your node to sell",
		choices=c("Mission control state (channel probes)"="routermc")
	)
}

#' main layout UI for various earn elements
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return return the main layout UI element
#' @export
earnUI <- function(id) {
	ns <- NS(id)
	fluidRow(
		tags$head(
			tags$style(
				HTML("
					#shiny-notification-panel {
						top: 50px;
						bottom: unset;
						right: 0;
						margin-left: auto;
						margin-right: auto;
						max-width: 350px;
					}
				")
			)
		),
		useShinyjs(),
		extendShinyjs(text=detectProvider(ns("webln_detect")), functions=c()),
		extendShinyjs(text=requestRoutermc(ns("webln_request_routermc")), functions=c("webln_request_routermc")),
		column(12, offset=2,
			conditionalPanel(
				condition="output.account_is_auth == 'false'", ns=ns,
				box(title=NULL, background='yellow', width=8,
					h2("Earn sats for your data", align="center"),
					br(),
					column(12, align="center",
						upgradeButtonUI(NS(id, "ad_upgrade"))
					),
					br(),
					h4(HTML(paste("Requires a WebLN provider like", a("Alby", href="https://getalby.com/", target="_blank"))))
				)
			)
		),
		conditionalPanel(
			condition="output.account_is_auth == 'true'", ns=ns,
			column(8,
				box(id=NS(id, 'select.box'), background='yellow', width=NULL,
					column(12,
						nodeDataSelectUI(NS(id, 'data_select'))
					),
					column(12, align='center',
						startButtonUI(id=NS(id, 'data_upload_request'), buttonId="data_upload_start_button", lab="Initiate sale")
					),
				),
			),
			column(12,
				h3("Submission history"),
				dataTableUI(NS(id, "show_data_submissions"), "data_submission_table")
			)
		),
	)
}

#' submitted table information server
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials login status from \link{loginServer}
#' @param db sql db
#' @return returns renderTable server for account information
#' @export
submissionSummaryTableServer <- function(id, credentials, db=pool) {
	moduleServer(id, function(input, output, session) {
		output$data_submission_table <- renderDataTable({
			tbl(db, "payments") %>%
				filter(submitter==!!credentials()$info[1]$pubkey) %>%
				dplyr::select(date_submitted, amount, data_type, hash) %>%
				collect %>%
				dplyr::rename('Submission date'='date_submitted', 'Amount paid (sats)'='amount', 'Data type submitted'='data_type', 'Payment hash'='hash')
		})
	})
}

#' node data server
#'
#' returns the value selected in node data UI element
#'
#' @param id An ID string that corresponds with the ID used to call in
#' node data select UI elements
#' @return returns data choice
#' @export
nodeDataServer <- function(id) {
	moduleServer(id, function(input, output, session) {
		reactive(input$node_data)
	})
}

#' data handling server
#'
#' module for treating the data submitted by a user
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return return a tibble from the graph containing the simulation results
#' @export
userDataHandlingServer <- function(id, data_received, credentials, selected_data_type) {
	moduleServer(id, function(input, output, session) {
		data <- reactiveValues(parsed=NULL)
		observeEvent(data_received(), {
			req(data_received())
			if (selected_data_type() == "routermc") {
				showModal(
					modalDialog(
						title="Working on the request...",
						withSpinner(uiOutput("loading"), size=2),
						size='m',
						footer=NULL
					)
				)
				data$parsed <- data_received() %>%
					bind_rows %>%
					mutate(name=names(history), value=as.numeric(history)) %>%
					dplyr::select(-history) %>%
					pivot_wider(names_from=name, values_from=value) %>%
					mutate(
						date_submitted=now(tzone="UTC"),
						submitter=credentials()$info[1]$pubkey,
						time=ifelse(success_time>0, success_time, fail_time),
						amt=ifelse(success_time>0, success_amt_sat, fail_amt_sat),
						htlc=ifelse(success_time>0, "success", "fail")) %>%
					dplyr::rename('from'='node_from', 'to'='node_to') %>%
					dplyr::select(submitter, date_submitted, time, from, to, amt, htlc) %>%
					mutate(id=row_number()) %>%
					group_by(id) %>%
					mutate_at(vars(from, to), list(~paste0(openssl::base64_decode(.), collapse=""))) %>%
					ungroup %>%
					dplyr::select(-id)
				removeModal()
			}
			# else other types of data
		})

		return(reactive(data$parsed))
	})
}

#' main earn page server
#'
#' backend handling of all account-related information
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials login status from \link{loginServer}
#' @export
earnServer <- function(id, credentials, db=pool) {
	moduleServer(id, function(input, output, session) {
		submissionSummaryTableServer("show_data_submissions", credentials, db)
		upgradeButtonServer("ad_upgrade", p(HTML("Login to start"), onclick="openTab('account')", style="text-align: center; height: 16px;"))
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

		selected_data_type <- nodeDataServer("data_select")
		observeEvent(startButtonServer("data_upload_request", "data_upload_start_button"), {
			if (!input$webln_detect) {
				showNotification("No WebLN provider detected. Try installing one like Alby", type="error")
			}
			else {
				if (selected_data_type() == "routermc") {
					user_last_submission <- tbl(db, "mc") %>%
						filter(submitter==!!credentials()$info[1]$pubkey) %>%
						filter(date_submitted==max(date_submitted)) %>%
						distinct(date_submitted) %>%
						pull
					if (!length(user_last_submission) || as.numeric(now()-user_last_submission, unit="days") > 1) {
						# initiate request for routermc data and activate input
						js$webln_request_routermc()
					} else {
						remaining <- 24 - round(as.numeric(now()-user_last_submission, unit="hours"), 0)
						showModal(
							modalDialog(
								tags$style(".fa-exclamation-triangle {color: #f39c12;}"),
								title=paste("You will be eligible to make another sale in about", remaining, "hours"),
								column(12, align="center", icon("exclamation-triangle", "fa-5x")),
								size='m',
								easyClose=TRUE,
								footer=modalButton("Done")
							)
						)
					}
				}
			}
		})

		initiated_data_req <- reactiveVal(NULL)
		observeEvent(input$webln_request_routermc, {
			if (!!length(input$webln_request_routermc) && !("catch" %in% names(input$webln_request_routermc))) {
				initiated_data_req(input$webln_request_routermc)
			}
			else if ("catch" %in% names(input$webln_request_routermc) && grepl("not supported by your account", input$webln_request_routermc$reason)) {
				showNotification("Only LND nodes are currently supported", type="error")
				initiated_data_req(FALSE)
			}
			else if ("catch" %in% names(input$webln_request_routermc) && grepl("Provider must be enabled", input$webln_request_routermc$reason)) {
				showNotification(input$webln_request_routermc$reason, type="warning")
				initiated_data_req(FALSE)
			}
			else {
				showNotification("WebLN transaction cancelled", type="warning")
				initiated_data_req(FALSE)
			}
		})

		data_receive <- userDataHandlingServer("webln_user_data", initiated_data_req, credentials, selected_data_type)
		inv_req <- weblnInvoiceRequestServer("inv_req", data_receive, credentials()$info[1]$pubkey, data_type="Router Mission Control")

		observeEvent(inv_req(), {
			if (inv_req()$status == "Complete") {
				dbWriteTable(db, "mc", data_receive(), append=TRUE, overwrite=FALSE)
				accounting_info <- data_receive() %>%
					dplyr::select(submitter, date_submitted) %>%
					unique %>%
					mutate(hash=inv_req()$hash, amount=inv_req()$amount/1e3, data_type="Router Mission Control")
				dbWriteTable(db, "payments", accounting_info, append=TRUE)
				submissionSummaryTableServer("show_data_submissions", credentials, db)
			}
		})
	})
}

#' earn page standalone
#'
#' for dev/testing purposes
earnApp <- function() {
	ui <- dashboardPage(
		dashboardHeader(title='Earn Page'),
		dashboardSidebar(),
		dashboardBody(earnUI('x')),
		skin='yellow',
	)
	credentials <- reactiveValues(
		info=data.frame(pubkey=test_pubkey, foo="bar"),
		premium=TRUE, user_auth=TRUE)
		#info=data.frame(pubkey="", foo="bar"),
		#user_auth=FALSE, premium=FALSE)
	server <- function(input, output, session) {
		earnServer('x', reactive(credentials))
	}
	shinyApp(ui, server)
  
}
