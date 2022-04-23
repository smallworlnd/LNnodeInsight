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
invoiceGenerator <- function(id, link, amt, desc) {
	moduleServer(id, function(input, output, session) {
		# build an invoice for the service
		inv_body <- toJSON(
			list(
				amount=format(as.numeric(amt), scientific=FALSE),
				description=desc,
				expiry=360,
				privateRouteHints=TRUE),
			auto_unbox=TRUE)
		inv <- content(POST(url=link, body=inv_body, config=store_headers))
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
				plotOutput(ns("inv_qr"), height='400px', width='400px'),
				br(),
				div(style="width: 400px;", verbatimTextOutput(ns("inv_text"))),
				title=desc,
				footer=tagList(
					#rclipButton(ns("clipbtn"), "Copy", invoice$BOLT11, icon("clipboard"), modal=TRUE),
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
invoiceHandlingServer <- function(id, reactive_trigger, inv_fetch_url, inv_amt, display_desc, inv_desc) {
	moduleServer(id, function(input, output, session) {
		invoice <- reactiveValues(details=NULL)
		observeEvent(reactive_trigger(), {
			invoice$details <- invoiceGenerator(paste0(id, "_inv"), inv_fetch_url, inv_amt, inv_desc)
			invoiceDisplayServer(paste0(id, "_inv"), invoice$details, display_desc, inv_amt)
		})

		inv_cancel <- invoiceCancel(paste0(id, "_inv"))
		observeEvent(inv_cancel(), {
			invoice$details$status <- "Cancelled"
		})
		observe({
			req(invoice$details)
			if (invoice$details$status != "Cancelled" && invoice$details$status == "Unpaid") {
				invoice$details$status <- content(GET(url=paste0(inv_fetch_url, '/', invoice$details$id), config=store_headers))$status
				invalidateLater(2000)
			} else {
				removeModal()
				return()
			}
		})
		return(reactive(invoice$details$status))
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

