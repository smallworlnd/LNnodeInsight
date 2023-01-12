#' Detect a webln provider installed in the browser
#'
#' @param id an id string used to call the script's server input
#' @return returns a boolean shiny input value
#' @export
detectProvider <- function(id) {
	glue::glue(.open = "{{{", .close = "}}}", '
	shinyjs.init = async function() {
		$(document).on("shiny:sessioninitialized", function(event) {
		  if (typeof window.webln !== "undefined") {
			Shiny.setInputValue("{{{id}}}", true);
		  } else {
			Shiny.setInputValue("{{{id}}}", false);
		  }}
		)
	};')
}

#' Opens the WebLN provider window to request permissions for use
#'
#' @param id an id string used to call the script's server input
#' @return returns a shiny input value containing user response
#' @export
requestProvider <- function(id) {
	glue::glue(.open = "{{{", .close = "}}}", '
	shinyjs.webln_enable = async function() {
	  try {
		const status = await window.webln.enable();
		Shiny.setInputValue("{{{id}}}", status.enabled);
	  }
	  catch(error) {
		Shiny.setInputValue("{{{id}}}", {catch: error.name, reason: error.message});
	  }
	};')
}

#' Request a message signature from WebLN provider
#'
#' @param id an id string used to call the script's server input
#' @return returns a shiny input value containing message signature info
#' @export
requestMessageSig <- function(id) {
	glue::glue(.open = "{{{", .close = "}}}", '
	shinyjs.webln_signmessage = async function(params) {
	  try {
		var defaultParams = {message: null};
		await window.webln.enable();
		params = shinyjs.getParams(params, defaultParams);
		const signed = await webln.signMessage(params.message);
		Shiny.setInputValue("{{{id}}}", signed);
	  } catch (error) {
		Shiny.setInputValue("{{{id}}}", {catch: error.name, reason: error.message});
	  }
	};')
}

#' Request a payment from the WebLN provider
#'
#' @param id an id string used to call the script's server input
#' @return returns a shiny input value containing payment info
#' @export
requestPayment <- function(id) {
	glue::glue(.open = "{{{", .close = "}}}", '
	shinyjs.webln_request_payment = async function(params) {
	  try {
		var defaultParams = {bolt11: null};
		await window.webln.enable();
		params = shinyjs.getParams(params, defaultParams);
		const pay_response = await window.webln.sendPayment(params.bolt11);
		Shiny.setInputValue("{{{id}}}", pay_response);
	  } catch (error) {
		Shiny.setInputValue("{{{id}}}", {catch: error.name, reason: error.message});
	  }
	};')
}
