source("inst/shiny-common.R")
source('inst/invoice-mgmt.R', local=TRUE)

#' main layout UI for various app elements
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return return the main layout UI element
#' @export
capfeesimUI <- function(id) {
	useShinyjs()
	ns <- NS(id)
	fluidRow(
		column(8,
			box(id=NS(id, 'select.box'), background='yellow', width=NULL,
				fluidRow(
					column(12,
						nodeSelectUI(NS(id, 'node_select'), listId="subject", lab="Enter/select your pubkey or alias"),
						nodeSelectUI(NS(id, 'node_select'), listId="target", lab="Enter/select the desired pubkey/alias to get capacity and fee recommendations for")
					),
				),
				column(12, align='center',
					startButtonUI(id=NS(id, 'launch_sim'), lab=startButtonLabel(NS(id, "start_sim")))
				),
			),
		),
		column(4,
			fluidRow(
				box(id=NS(id, 'sim.box'), title="Capacity-Fee suggestion summary",
					solidHeader=TRUE, collapsible=TRUE, width=12,
					lapply(
						data.frame(
							id=c("passive", "active", "min_cap", "ideal_cap"),
							tooltip=c(
								"This is the expected fee at which passive rebalancing may occur. If you see lots of traffic at fees above this rate then your liquidity is more valuable than most in the sampled paths, provided adequate balance is available. If you have little traffic at fees lower than this rate then your liquidity is less valuable there.",
								"This fee reflects the expected cost of actively rebalancing through the target node. If you see lots of traffic, provided adequate balance, at fees above this rate then you have valuable liquidity in that channel. If you have little traffic at fees lower than this rate then your liquidity is less valuable there.",
								"This value represents the minimally viable channel capacity based on maximum liquidity flows in paths to and from the target node. It is recommended to open a channel with capacity greater than this value.",
								"This value represents a more usable channel capacity based on maximum liquidity flows. A larger channel or additional channel to the same target node may also be useful depending on observed traffic, or even required by the node."
							)
						) %>% t %>% as.data.frame,
						function(x)
							simResultUI(NS(id, "sim_result"), x[1]) %>% bs_embed_tooltip(title=x[2], placement="auto")
					)
				),
				column(12, align="center",
					conditionalPanel(
						"output.is_premium == 'false'", ns=ns,
						upgradeButtonUI(NS(id, "ad_upgrade"))
					)
				)
			)
		)
	)

}

#' simulation server
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param subject subject pubkey on which simulations are run
#' @param api_info url and auth token (if present) of rebalance simulation
#' api/backend
#' @return return a tibble from the graph containing the simulation results
#' @export
capfeeSimulationServer <- function(id, subject, api_info) {
	moduleServer(id, function(input, output, session) {
		subject <- subject()
		# poorman's async request to chansim api by sending process to background
		# and avoid session locks
		sim_request <- callr::r_bg(
			func = function(subject_pubkey, api_info) {
				req_body <- jsonlite::toJSON(list(subject_pubkey=subject_pubkey), auto_unbox=TRUE)
				if ("token" %in% names(api_info)) {
					api_request <- googleCloudRunner::cr_jwt_with_httr(
						httr::POST(url=api_info$url, body=req_body, encode="json"),
						api_info$token)
				} else {
					api_request <- httr::POST(url=api_info$url, body=req_body, encode="json")
				}
				return(httr::content(api_request))
			},
			args=list(subject_pubkey=subject, api_info=api_info),
			supervise=TRUE
		)
		return(sim_request)
	})
}

#' simulation result output server
#'
#' backend for generating simulation result summary statistics; blank by default
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param resId result ID, e.g., for mean, or median, etc.
#' @param xvar corresponding column name to tabId in the simulation result table
#' @param desc description of the metric
#' @param sim_res_reactive reactive simulation results
#' @return returns a valuebox output for given summary statistic for a particular metric
#' @export
capfeeSimulationResultServer <- function(id, resId, xvar, desc, sim_res_reactive) {
	moduleServer(id, function(input, output, session) {
		sim_res <- reactiveVal(NULL)
		observeEvent(sim_res_reactive(), {
			sim_res(sim_res_reactive())
		})
		output[[resId]] <- renderValueBox({
			if (!is.null(sim_res())) {
				val <- eval(parse(text=paste0("sim_res()$", xvar))) %>%
					round(0) %>%
					format(scientific=FALSE) %>%
					prettyNum(big.mark=",")
			} else {
				val <- ""
			}
			valueBox(val, desc, color="blue")
		})
	})
}

#' main channel simulation server
#'
#' backend handling of all inputs/outputs for channel simulation
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials reactively hide/show some filters depending on user
#' account
#' @param api_info api url and auth token to POST channel simulation information
#' to local (if no auth token present) or remote backend
#' @export
capfeesimServer <- function(id, api_info, credentials, db=pool) {
	moduleServer(id, function(input, output, session) {
		# initializ reactive values
		users <- db %>% tbl('users')
		subject <- getNodePubkey('node_select', "subject")
		target <- getNodePubkey('node_select', "target")
		lapply(c("subject", "target"), function(x) nodeListServer("node_select", listId=x))

		is_premium <- premiumAccountReactive("prem_account", credentials, users)
		upgradeButtonServer("ad_upgrade",
			p(HTML("Want to automatically run this tool on potential peers?<br/>Sign up!"), onclick="openTab('account')"))
		output$is_premium <- premiumAccountReactive("prem_account", credentials, users)
		outputOptions(output, "is_premium", suspendWhenHidden=FALSE)
		startButtonLabelServer("start_sim", paste('View suggestions for', as.numeric(Sys.getenv("CAPFEESIM_MSAT"))/1e3, "sats"), is_premium)

		# start simulation reactive button
		sim_start_button <- startButtonServer("launch_sim", "launch_sim_button")

		# start the simulation when the start button is selected
		sim_run <- eventReactive(sim_start_button(), {
			req(target() != "", subject() != "")
			showModal(
				modalDialog(
					title="Running simulation, this may take up to several minutes. Please wait...",
					withSpinner(uiOutput("loading"), size=2),
					size='s', footer=NULL
				)
			)
			# run a new sim if no previous result exists
			# or if last result is older than 7 days
			capfeeSimulationServer("launch_sim", target, api_info)
		})
		# ping the background process and fetch results when done
		sim_result_nosub <- reactiveVal(NULL) # reactive for non-premium
		sim_result_sub <- reactiveVal(NULL) # reactive for premium
		observe({
			req(sim_run())
			if (isolate(sim_run()$is_alive())) {
				invalidateLater(1000)
			} else {
				# short-circuit invoicing if account is premium
				sim_res_from_db <- isolate(tbl(db, "capfee") %>%
					filter(pubkey==!!target()) %>%
					filter(time==max(time)) %>%
					as_tibble)
				if (is_premium() == "true") {
					isolate(sim_result_sub(sim_res_from_db))
				} else {
					isolate(sim_result_nosub(sim_res_from_db))
				}
				removeModal()
			}
		})
		# generate and manage invoice once simulation is done if not premium
		invoice <- invoiceHandlingServer(
			"capfeesim_inv",
			reactive_trigger=sim_result_nosub,
			inv_fetch_url=Sys.getenv("STORE_URL"),
			inv_amt=Sys.getenv("CAPFEESIM_MSAT"),
			display_desc=paste("Done! Please pay", as.numeric(Sys.getenv("CAPFEESIM_MSAT"))/1e3, "sats to view results."),
			inv_desc="capfee simulator")
		# display results if premium, else require that the invoice be paid to
		# reactively show simulation results
		sim_output <- eventReactive(c(invoice(), sim_result_sub()), {
			if (is_premium() == "true") {
				sim_result_sub()
			} else {
				req(invoice() == "Paid")
				sim_result_nosub()
			}
		})
		# reactive output summary stats depending on active histogram tab
		lapply(
			data.frame(
				resId=c("passive", "active", "min_cap", "ideal_cap"),
				desc=c("Strategy 1: passive rebalancing fee (ppm)", "Strategy 2: active rebalancing fee (ppm)", "Minimally viable capacity (sat)", "Minimum suggested capacity (sat)")
			) %>% t %>% as.data.frame,
			function(x)
				capfeeSimulationResultServer(id="sim_result", resId=x[1], xvar=x[1], desc=x[2], sim_output)
		)
	})
}

#' channel simulation app standalone
#'
#' for dev/testing purposes
capfeesimApp <- function() {
	capfeesim_api_info <- if (Sys.getenv("LOCAL")) {
			list(url=Sys.getenv("CAPFEESIM_LOCAL_API_URL"))
		} else {
			get_api_info("capfeesim-api")
		}
	ui <- dashboardPage(
		dashboardHeader(title='Capacity-Fee Simulator'),
		dashboardSidebar(),
		dashboardBody(capfeesimUI('x')),
		skin='yellow',
	)
	credentials <- reactiveValues(
		info=data.frame(pubkey=test_pubkey, foo="bar"),
		user_auth=TRUE, cookie_already_checked=FALSE)
		#info=NULL,
		#user_auth=FALSE, cookie_already_checked=FALSE)
	server <- function(input, output, session) {
		capfeesimServer('x', capfeesim_api_info, reactive(reactiveValuesToList(credentials)))
	}
	shinyApp(ui, server)
}
