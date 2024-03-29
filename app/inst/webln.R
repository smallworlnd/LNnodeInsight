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
requestLoginEnable <- function(id) {
	glue::glue(.open = "{{{", .close = "}}}", '
	shinyjs.webln_login_enable = async function() {
	  try {
		await window.webln.enable();
		Shiny.setInputValue("{{{id}}}", true);
	  }
	  catch(error) {
		Shiny.setInputValue("{{{id}}}", {catch: error.name, reason: error.message});
	  }
	};')
}
requestEarnEnable <- function(id) {
	glue::glue(.open = "{{{", .close = "}}}", '
	shinyjs.webln_earn_enable = async function() {
	  try {
		await window.webln.enable();
		Shiny.setInputValue("{{{id}}}", true);
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
		const response = await window.webln.sendPayment(params.bolt11);
		Shiny.setInputValue("{{{id}}}", response);
	  } catch (error) {
		Shiny.setInputValue("{{{id}}}", {catch: error.name, reason: error.message});
	  }
	};')
}

#' Request routermc data from WebLN provider
#'
#' @param id an id string used to call the script's server input
#' @return returns a shiny input value containing routermc info
#' @export
requestRoutermc <- function(id) {
	glue::glue(.open = "{{{", .close = "}}}", '
	shinyjs.webln_request_routermc = async function() {
	  try {
		await window.webln.enable();
	    const routermc_response = await webln.request("routermc");
		Shiny.setInputValue("{{{id}}}", routermc_response);
	  } catch (error) {
		Shiny.setInputValue("{{{id}}}", {catch: error.name, reason: error.message});
	  }
	};')
}

#' Request an invoice from the WebLN provider
#'
#' @param id an id string used to call the script's server input
#' @return returns a shiny input value containing invoice info
#' @export
requestInvoice <- function(id) {
	glue::glue(.open = "{{{", .close = "}}}", '
	shinyjs.webln_request_invoice = async function(params) {
	  try {
		var defaultParams = {amount: null, defaultMemo: null};
		await window.webln.enable();
		params = shinyjs.getParams(params, defaultParams);
		const response = await window.webln.makeInvoice({amount: params.amount, defaultMemo: params.defaultMemo});
		Shiny.setInputValue("{{{id}}}", response);
	  } catch (error) {
		Shiny.setInputValue("{{{id}}}", {catch: error.name, reason: error.message});
	  }
	};')
}
