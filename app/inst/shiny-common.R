#' start button UI element
#'
#' basic start button used to initialize simulations
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param lab label displayed in the middle of the button, e.g. "Start"
#' @return returns a small and simple rectangular UI element
#' @export
startButtonUI <- function(id, buttonId="launch_sim_button", lab, button_color="success") {
	actionBttn(
		inputId=NS(id, buttonId),
		label=lab,
		style='fill',
		color=button_color,
		block=FALSE
	)
}

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
nodeSelectUI <- function(id, listId, lab) {
	selectizeInput(
		inputId=NS(id, listId),
		label=lab,
		choices=NULL,
		options=list(placeholder='Pubkey/alias')
	)
}

#' node list server
#'
#' server-side updating of node list, connected with \link{nodeSelectUI}
#'
#' @param id An ID string that corresponds with the ID used to call in
#' \link{chansimUI} or \link{rebalsimUI}, for example
#' @param listId list id connected to node selection UI element,
#' \link{nodeSelectUI}
#' @param pubkey_list full list of nodes to serve
#' @return returns updateSelectizeInput server element with node list
#' @export
nodeListServer <- function(id, listId, pubkey_list=node_ids, default_selected=character(0)) {
	moduleServer(id, function(input, output, session) {
		observe({
			isolate(updateSelectizeInput(
				session,
				inputId=listId,
				choices=c("Pubkey or alias"=NULL, pubkey_list),
				selected=default_selected,
				server=TRUE
			))
		})
	})
}

#' node pubkey parser
#'
#' simple function that takes input in the form of "alias - pubkey" and parses
#' the pubkey
#'
#' @param id An ID string that corresponds with the ID used to call in
#' node/subject/target select UI elements
#' @param node node ID, e.g., "subject" or "out_node" in rebalance simulator
#' @return returns parsed pubkey from input string
#' @export
getNodePubkey <- function(id, node) {
	moduleServer(id, function(input, output, session) {
		reactive(fetch_pubkey(eval(parse(text=paste0('input$', node)))))
	})
}

#' custom modalButton
#'
#' adapted modalButton for custom labelling and ID'g to connect with other
#' server/UI elements
#'
#' @param inputId id with which to connect to other UI/server elements
#' @param label label for the custom button, in this app only "Cancel" is ever
#' used
#' @param icon icon like with modalButton
#' @param width width like with modalButton
#' @param ... same as for modalButton
#' @return returns modified modalButton that can be linked to other elements
#' @export
modalActionButton <- function(inputId, label, icon = NULL, width = NULL, ...) {
	value <- restoreInput(id = inputId, default = NULL)
	tags$button(id = inputId, type = "button", style = if (!is.null(width)) 
	paste0("width: ", validateCssUnit(width), ";"), type = "button", 
	class = "btn btn-default action-button", `data-dismiss` = "modal", `data-val` = value, 
	list(shiny:::validateIcon(icon), label), ...)
}

#' start button server
#'
#' generic module for reacting to new input from \link{startButtonUI}
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#' UI element, \link{startButtonUI}
#' @param buttonId input ID for the given button scoped to the app, e.g.,
#' channel simulator or rebalance simulator
#' @return returns input from \link{startButtonUI} clicks
#' @export
startButtonServer <- function(id, buttonId) {
	moduleServer(id, function(input, output, session) {
		reactive(eval(parse(text=paste0("input$", buttonId))))
	})
}

#' fetch account status
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI element, \link{startButtonUI}
#' @param credentials login status from \link{loginServer}
#' @param db sql db
#' @return returns character string "true" or "false" (instead of boolean, for
#' server->client outputOptions
#' @export
premiumAccountReactive <- function(id, credentials, db) {
	moduleServer(id, function(input, output, session) {
		reactive({
			if (!credentials()$user_auth) {
				return("false")
			} else {
				account <- tbl(db, "users") %>%
					filter(pubkey==!!credentials()$info[1]$pubkey) %>%
					filter(sub_date==max(sub_date)) %>%
					filter(subscription=="Premium") %>%
					as_tibble %>%
					distinct(sub_date, .keep_all=TRUE)
				if (nrow(account) > 0 && account$subscription == "Premium" && account$sub_expiration_date>=now()) {
					return("true")
				} else {
					return("false")
				}
			}
		})
	})
}

#' account upgrade button UI ielement
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return returns ui element for action button
#' @export
upgradeButtonUI <- function(id) {
	uiOutput(NS(id, "upgrade_button"))
}

#' account upgrade button server
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param msg message to show in action button label
#' @return returns infobox rendering
#' @export
upgradeButtonServer <- function(id, msg) {
	moduleServer(id, function(input, output, session) {
		output$upgrade_button <- renderUI({
			actionBttn(inputId='upgrade_button', label=msg, style='fill', color='success', block=FALSE, size='sm')
		})
	})
}

#' fetch simulator api info
#'
#' fetch the url and token (if doing authenticated remote calls) for the
#' simulator backends/APIs
#'
#' @param api_name name of the API, currently either 'rebalsim-api' or
#' 'chansim-api'
#' @return returns API url and auth token (if found)
get_api_info <- function(api_name) {
	api_url <- cr_run_get(api_name)$status$url
	jwt <- cr_jwt_create(api_url)
	token <- cr_jwt_token(jwt, api_url)
	return(list(url=api_url, token=token))
}

#' start/action button label UI element
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return returns text label for action button depending on account status
#' @export
startButtonLabel <- function(id) {
	textOutput(NS(id, "account_is_premium"))
}

#' start/action button label server
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param account_check_reactive reactive element to verify account status
#' @return returns render text element depending on account status
startButtonLabelServer <- function(id, custom_label, account_check_reactive) {
	moduleServer(id, function(input, output, session) {
		output$account_is_premium <- renderText({
			if (account_check_reactive() == "true") {
				paste("Start")
			} else {
				paste(custom_label)
			}
		})
	})
}

#' UI element for simulation summary statistic
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param resId summary statistic result ID, e.g., mean, median, sd, etc.
#' @return returns value box output UI element for the given summary statistic
#' @export
simResultUI <- function(id, resId) {
	valueBoxOutput(NS(id, resId), width=12)
}

#' generic tab plot UI element
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param plotTitle title of the plot displayed in tab
#' @param plotId id of the plot element connected to its corresponding server
#' module
#' @param plotType the type of plot output, usualyl plotlyOutput
#' @return returns generic plot UI element
#' @export
plotOutputUI <- function(id, plotTitle, plotId, plotType) {
	tabPanel(
		plotTitle,
		withSpinner(
			eval(
				parse(text=paste0(plotType, "(NS(id, \"", plotId, "\"))"))
			)
		),
		value=plotId,
		id=NS(id, paste0(plotId, '_tab')),
		width=NULL
	)
}
