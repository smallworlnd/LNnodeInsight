nodeSelectUI <- function(id, listId, lab) {
	selectizeInput(
		inputId=NS(id, listId),
		label=lab,
		choices=NULL,
		options=list(placeholder='Pubkey/alias')
	)
}

nodeListServer <- function(id, listId, pubkey_list=node_ids) {
	moduleServer(id, function(input, output, session) {
		updateSelectizeInput(
			session,
			inputId=listId,
			choices=c("Pubkey or alias"=NULL, pubkey_list),
			selected=character(0),
			server=TRUE
		)
	})
}

getNodePubkey <- function(id, node) {
	moduleServer(id, function(input, output, session) {
		reactive(fetch_pubkey(eval(parse(text=paste0('input$', node)))))
	})
}

modalActionButton <- function(inputId, label, icon = NULL, width = NULL, ...) {
	value <- restoreInput(id = inputId, default = NULL)
	tags$button(id = inputId, type = "button", style = if (!is.null(width)) 
	paste0("width: ", validateCssUnit(width), ";"), type = "button", 
	class = "btn btn-default action-button", `data-dismiss` = "modal", `data-val` = value, 
	list(shiny:::validateIcon(icon), label), ...)
}

startButtonServer <- function(id, buttonId) {
	moduleServer(id, function(input, output, session) {
		reactive(eval(parse(text=paste0("input$", buttonId))))
	})
}
