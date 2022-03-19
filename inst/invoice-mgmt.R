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

invoiceDisplayServer <- function(id, invoice, amt) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		output$inv_qr <- renderPlot({
			ggqrcode(invoice$BOLT11)
		})
		qrModal <- function() {
			modalDialog(
				plotOutput(ns("inv_qr"), height='270px', width='270px'),
				title=paste("Done! Please pay", as.numeric(amt)/1e3, "sats to view results."),
				size='s',
				footer=tagList(
					rclipButton(ns("clipbtn"), "Copy", invoice$BOLT11, icon("clipboard"), modal=TRUE),
					modalActionButton(ns("cancel"), "Cancel")
				)
			)
		}
		showModal(qrModal())
	})
}

invoiceHandlingServer <- function(id, reactive_trigger, inv_fetch_url, inv_amt, inv_desc) {
	moduleServer(id, function(input, output, session) {
		invoice <- reactiveValues(details=NULL)
		observeEvent(reactive_trigger(), {
			invoice$details <- invoiceGenerator(paste0(id, "_inv"), inv_fetch_url, inv_amt, inv_desc)
			invoiceDisplayServer(paste0(id, "_inv"), invoice$details, inv_amt)
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

invoiceCancel <- function(id) {
	moduleServer(id, function(input, output, session) {
		return(reactive(input$cancel))
	})
}

