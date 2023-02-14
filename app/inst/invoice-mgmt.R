#' invoice generator
#'
#' needs access to a btcpay server api to create invoices
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param link url to the btcpay create invoice api
#' @param amt create invoice for this amount in msat
#' @param desc description for the invoice
#' @return returns a list of the invoice details
#' @export
invoiceGenerator <- function(id, link, amt, desc, header=store_headers) {
	moduleServer(id, function(input, output, session) {
		# build an invoice for the service
		inv_body <- toJSON(
			list(
				amount=format(as.numeric(amt), scientific=FALSE),
				description=desc,
				expiry=360,
				privateRouteHints=TRUE),
			auto_unbox=TRUE)
		inv <- content(POST(url=link, body=inv_body, config=header))
		return(inv)
	})
}

#' invoice display server
#'
#' modal dialog to display a qr code of the bolt11 invoice
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param invoice invoice generated with \link{invoiceGenerator}
#' @param desc descriptive header for the modal shown above the qr
#' @param amt amount (in sats) the invoice is for
#' @return returns modal dialog UI element displaying qr of the bolt11 invoice
#' @export
invoiceDisplayServer <- function(id, invoice, desc, amt) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		output$inv_qr <- renderPlot({
			ggqrcode(invoice$BOLT11)
		})
		output$inv_text <- renderText({
			invoice$BOLT11
		})
		qrModal <- function() {
			modalDialog(
				useShinyjs(),
				extendShinyjs(text=requestPayment(ns("webln_request_payment")), functions=c("webln_request_payment")),
				plotOutput(ns("inv_qr"), height='400px', width='400px'),
				br(),
				div(style="width: 400px;", verbatimTextOutput(ns("inv_text"))),
				footer=tagList(
					#rclipButton(ns("clipbtn"), "Copy", invoice$BOLT11, icon("clipboard"), modal=TRUE),
					actionButton(ns("webln_pay"), "Pay with WebLN provider"),
					modalActionButton(ns("cancel"), "Cancel")
				)
			)
		}
		showModal(qrModal())
	})
}

#' invoice handling server
#'
#' generators a bolt11 invoice with \link{invoiceGenerator}, displays the UI
#' element for the invoice with \link{invoiceDisplayServer} and pings the btcpay
#' api for invoice status every 2s.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param reactive_trigger trigger to start the invoice handling process
#' @param inv_fetch_url url to the btcpay server api
#' @param inv_amt amount (in sats) the invoice is for
#' @param display_desc description given to \link{invoiceDisplayServer} to
#' display in the modal header
#' @param inv_desc invoice description
#' @return returns a reactive of the invoice status
#' @export
invoiceHandlingServer <- function(id, reactive_trigger, inv_fetch_url, inv_amt, display_desc, inv_desc, header=store_headers) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns

		invoice <- reactiveValues(details=NULL)
		observeEvent(reactive_trigger(), {
			invoice$details <- invoiceGenerator(paste0(id, "_inv"), inv_fetch_url, inv_amt, inv_desc)
			invoiceDisplayServer(paste0(id, "_inv"), invoice$details, display_desc, inv_amt)
		})

		inv_cancel <- invoiceCancel(paste0(id, "_inv"))
		observeEvent(inv_cancel(), {
			invoice$details$status <- "Cancelled"
		})

		webln_pay_button_click <- weblnModalPayButton(paste0(id, "_inv"))
		webln_pay_detect <- weblnPayInput(paste0(id, "_inv"))
		observeEvent(webln_pay_button_click(), {
			js$webln_request_payment(invoice$details$BOLT11)
		})

		observeEvent(webln_pay_detect(), {
			if ("catch" %in% names(webln_pay_detect())) {
				if (webln_pay_detect()$catch == "TypeError") {
					showNotification("No WebLN provider detected, try installing one like Alby", type="error")
				} else {
					showNotification("WebLN payment canceled", type="warning")
				}
			}
		})

		observe({
			req(invoice$details)
			if (invoice$details$status != "Cancelled" && invoice$details$status == "Unpaid") {
				invoice$details$status <- content(GET(url=paste0(inv_fetch_url, '/', invoice$details$id), config=header))$status
				invalidateLater(2000)
			} else {
				removeModal()
				return()
			}
		})
		return(reactive(invoice$details))
	})
}

#' invoice cancel server
#'
#' when the invoice modal is active, provide a cancel button to stop the invoice
#' handling process
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns a reactive input on "cancel" click
#' @export
invoiceCancel <- function(id) {
	moduleServer(id, function(input, output, session) {
		return(reactive(input$cancel))
	})
}

#' pay by webln
#'
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns a reactive input on "pay by webln" click
#' @export
weblnModalPayButton <- function(id) {
	moduleServer(id, function(input, output, session) {
		return(reactive(input$webln_pay))
	})
}

#' get input from a request to pay via webln provider
#'
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns a reactive input on webln payment request
#' @export
weblnPayInput <- function(id) {
	moduleServer(id, function(input, output, session) {
		return(reactive(input$webln_request_payment))
	})
}

#' sell by webln
#'
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns a reactive input on "pay by webln" click
#' @export
weblnModalSellButton <- function(id) {
	moduleServer(id, function(input, output, session) {
		return(reactive(input$webln_sell))
	})
}

#' get input from an invoice request via webln provider
#'
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns a reactive input on webln invoice request
#' @export
weblnSellInput <- function(id) {
	moduleServer(id, function(input, output, session) {
		return(reactive(input$webln_request_invoice))
	})
}

#' invoice request modal dialog
#'
#' modal dialog that describes why the invoice is being requested
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param desc descriptive header for the modal shown above the qr
#' @param amt amount (in sats) the invoice is for
#' @return returns modal dialog UI element displaying qr of the bolt11 invoice
#' @export
weblnInvoiceRequestDialog <- function(id, amt) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		showModal(
			modalDialog(
				useShinyjs(),
				extendShinyjs(text=requestInvoice(ns("webln_request_invoice")), functions=c("webln_request_invoice")),
				title=paste("We will offer", amt, "sats for this specific submission of Mission Control data"),
				HTML(
					"Next step:
					<ul>
					<li>Create an invoice for LNnodeInsight to pay you if you accept</li>
					<li>Or cancel the transaction</li>
					</ul>"),
				footer=tagList(
					actionButton(ns("webln_sell"), "Create invoice for LNnodeInsight to pay"),
					modalActionButton(ns("cancel"), "Cancel")
				)
			)
		)
	})
}

#' webln invoice request handling server
#'
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param input_data input data trigger to start the invoice handling process
#' @param inv_fetch_url url to the btcpay server api
#' @param inv_amt amount (in sats) the invoice is for
#' @param display_desc description given to \link{invoiceDisplayServer} to
#' display in the modal header
#' @param inv_desc invoice description
#' @return returns a reactive of the invoice status
#' @export
weblnInvoiceRequestServer <- function(id, input_data, pubkey, data_type="Router Mission Control", db=pool, url_header=pay_headers) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		pay_inv_result <- reactiveVal(NULL)
		price <- reactiveVal(NULL)
		observeEvent(input_data(), {
			tmp <<- input_data()
			price(price_function(input_data(), db, pubkey))
			if (price() > 0) {
				weblnInvoiceRequestDialog(paste0(id, "_inv"), price()) 
			} else {
				showModal(
					modalDialog(
						title="Not enough new data to receive the minimal payment amount",
						"You may need to wait more time to accumulate more data",
						size='m',
						easyClose=TRUE,
						footer=modalButton("Done")
					)
				)
			}
		})

		webln_sell_button_click <- weblnModalSellButton(paste0(id, "_inv"))
		webln_pay_user_invoice <- weblnSellInput(paste0(id, "_inv"))
		observeEvent(webln_sell_button_click(), {
			if (data_type == "Router Mission Control") {
				js$webln_request_invoice(
					amount=price(),
					defaultMemo=paste("Payment from lnnodeinsight.com for", data_type, "data"))
			}
		})

		pay_inv <- eventReactive(webln_pay_user_invoice(), {
			if ("catch" %in% names(webln_pay_user_invoice()) && webln_pay_user_invoice()$reason == "User rejected") {
				showNotification("WebLN invoice creation canceled", type="warning")
				removeModal()
			}
			req("paymentRequest" %in% names(webln_pay_user_invoice()))
			showModal(
				modalDialog(
					title="Attempting to pay invoice...",
					withSpinner(uiOutput("loading"), size=2),
					size='s',
					footer=NULL
				)
			)
			invoicePayServer(paste0(id, "_inv"), url_header, webln_pay_user_invoice()$paymentRequest, price())
		})

		observe({
			req(pay_inv())
			if (isolate(pay_inv()$is_alive())) {
				invalidateLater(500)
			} else {
				pay_inv_result(payStatusHandlingServer(paste0(id, "_inv"), pay_inv()))
				showModal(
					modalDialog(
						tags$style(".fa-check {color: #28b78d}"),
						title=pay_inv_result()$status,
						column(12, align="center", pay_inv_result()$message),
						br(), br(),
						p(ifelse(pay_inv_result()$status=="Complete",
							"Next eligible submission is in 24 hours",
							"Please contact us about this error"),
							style="text-align: right"),
						size='s',
						easyClose=TRUE,
						footer=modalButton("Done")
					)
				)
			}
		})

		return(pay_inv_result)
	})
}

#' invoice payment server
#'
#' module for paying an invoice generated by user through webln
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return return a tibble from the graph containing the simulation results
#' @export
invoicePayServer <- function(id, url_header=pay_headers, bolt11, amt) {
	moduleServer(id, function(input, output, session) {
		callr::r_bg(
			func = function(pay_url, url_header, bolt11, price) {
				invoice_content <- jsonlite::toJSON(
					list(BOLT11=bolt11,
						amount=as.character(price),
						maxFeePercent="0.5"),
					auto_unbox=TRUE)
				pay_inv <- tryCatch({
						httr::POST(url=pay_url, body=invoice_content, config=url_header)
					},
					error = function(e) {
						return(e)
					})
				return(pay_inv)
			},
			args=list(pay_url=Sys.getenv("PAY_URL"), url_header=url_header, bolt11=bolt11, price=amt),
			supervise=TRUE
		)
	})
}

#' invoice payment status code handling server
#'
#' module for paying an invoice generated by user through webln
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return return a tibble from the graph containing the simulation results
#' @export
payStatusHandlingServer <- function(id, payment_reactive) {
	moduleServer(id, function(input, output, session) {
		pay_status_code <- status_code(payment_reactive$get_result())
		payment_resp_content <- content(payment_reactive$get_result())
		pay_result <- list(status=NULL, message=NULL, hash=NULL, amount=NULL)
		if (pay_status_code == 200 || pay_status_code == 202) {
			pay_result$status <- payment_resp_content$status
			if (pay_result$status == "Complete") {
				pay_result$message <- icon("check", "fa-5x")
				pay_result$hash <- payment_resp_content$paymentHash
				pay_result$amount <- as.numeric(payment_resp_content$totalAmount) - as.numeric(payment_resp_content$feeAmount)
			}
			else if (pay_result$status == "Failed") {
				pay_result$message <- "Something went wrong with the payment. Please contact us to follow up."
			}
			else if (pay_result$status == "Pending") {
				pay_result$message <- "The payment appears to be stuck pending. Please confirm payment receipt on your node."
			}
			else {
				pay_result$message <- "Something unexpected happened. Please contact us to follow up."
			}
		}
		else if (pay_status_code == 400) {
			pay_result$status <- "Error"
			pay_result$message <- content(payment_reactive$get_result())$message
		}
		else if (pay_status_code == 404) {
			pay_result$status <- "Error"
			pay_result$message <- "Something went wrong with the LNnodeInsight Lightning node"
		}
		else if (pay_status_code == 422) {
			pay_result$status <- "Error"
			pay_result$message <- content(payment_reactive$get_result())$message
		}
		else if (pay_status_code == 503) {
			pay_result$status <- "Error"
			pay_result$message <- "Unable to access the LNnodeInsight Lightning node"
		}
		return(pay_result)
	})
}
