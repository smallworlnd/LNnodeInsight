source("inst/shiny-common.R", local=TRUE)
source('inst/invoice-mgmt.R', local=TRUE)

#' UI element to choose payment or rebalance
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return radio button UI element to provide choice between rebalance or
#' payment
#' @export
simTypeUI <- function(id) {
	prettyRadioButtons(
		inputId=NS(id, 'pay_or_rebal'),
		label=HTML("Simulate either a rebalance or a payment<br>*Can be with existing or simulated channels"),
		selected=1,
		choiceNames=c('Rebalance', 'Payment'),
		choiceValues=c(1, 2),
		inline=TRUE
	)
}

#' UI tabPanel element to display histogram plot
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param plotTitle plot title displayed in the tab
#' @param plotId id specific to the plot output, e.g., path cost, max flow,
#' channel balance
#' @return returns histogram UI
#' @export
histoTabUI <- function(id, plotTitle, plotId) {
	tabPanel(
		title=plotTitle,
		withSpinner(plotlyOutput(NS(id, paste0('histo_', plotId, '_plot')))),
		value=paste0('histo_', plotId),
		id=NS(id, paste0("histo_", plotId, "_tab")),
		width=NULL
	)
}

#' UI tabPanel element to display scatterplot
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param plotTitle plot title displayed in the tab
#' @param plotId id specific to the plot output, e.g., path cost vs maxflow,
#' maxflow vs balance, etc
#' @return returns scatterplot UI
#' @export
scatterTabUI <- function(id, plotTitle, plotId) {
	tabPanel(
		plotTitle,
		withSpinner(plotlyOutput(NS(id, paste0('scatter_', plotId, '_plot')))),
		value=paste0('scatter_', plotId),
		id=NS(id, paste0('scatter_', plotId, '_tab')),
		width=NULL
	)
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

#' main layout UI for various rebalance-simulator app elements
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return return the main layout UI element
#' @export
rebalsimUI <- function(id) {
	ns <- NS(id)
	fluidRow(
		useShinyjs(),
		rclipboardSetup(),
		column(8,
			fluidRow(
				box(
					simTypeUI(NS(id, "sim_type")),
					conditionalPanel(
						"output.sim_type_choice == 1", ns=ns,
						# simulating a rebalance
						nodeSelectUI(NS(id, "node_select"), listId="subject", lab="Enter a pubkey/alias on which to simulate a circular rebalance")
					),
					nodeSelectUI(NS(id, "node_select"), listId="out_node", lab="Enter/choose an outgoing node"),
					nodeSelectUI(NS(id, "node_select"), listId="in_node", lab="Enter/select an incoming node"),
					column(12, align='center', startButtonUI(NS(id, "launch_sim"), lab=paste('View simulation results for', as.numeric(Sys.getenv("REBALSIM_MSAT"))/1e3, "sats"))),
					background='yellow', width=12,
				),
				do.call(tabBox,
					c(id=NS(id, 'histo_tab'), side='left', width=12,
						lapply(
							data.frame(
								plotTitle=c(
									"Path cost histogram",
									"Path maximum liquidity flow histogram",
									"High liquidity availability histogram"),
								plotId=c("cost", "flow", "bal")) %>% t %>% as.data.frame,
							function(x) histoTabUI(NS(id, "histo_tab_selected"), plotTitle=x[1], plotId=x[2])
						) %>% unname
					)
				),
				do.call(tabBox,
					c(id=NS(id, 'scatter_tab'), side='left', width=12,
						lapply(
							data.frame(
								plotTitle=c(
									'Maximum liquidity flow vs cost',
									'Maximum liquidity flow vs high liquidity availability',
									'High liquidity availability vs cost'),
								plotId=c("flowcost", "flowbal", "balcost")) %>% t %>% as.data.frame,
							function(x) scatterTabUI(NS(id, "scatter_tab_selected"), plotTitle=x[1], plotId=x[2])
						) %>% unname
					)
				)
			)
		),
		column(4,
			fluidRow(
				box(title="Summary stats", solidHeader=TRUE, collapsible=TRUE, width=NULL,
					lapply(
						c("samples", "min", "max", "mean", "median", "sd"),
						function(x)
							simResultUI(NS(id, "sim_result"), x)
					)
				)
			)
		)
	)
}

#' rebalance simulation server
#'
#' module for computing the path cost, maximum liquidity flow and channel
#' balancedness for a sub-population of all paths between out_node and in_node
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param subject subject pubkey on which simulations are run
#' @param out_node initiating node
#' @param in_node final (or last hop) node
#' @param api_info url and auth token (if present) of rebalance simulation
#' api/backend
#' @return return a tibble from the graph containing the simulation results
#' @export
simulationServer <- function(id, subject, out_node, in_node, api_info) {
	moduleServer(id, function(input, output, session) {
		subject <- subject()
		out_node <- out_node()
		in_node <- in_node()

		# poorman's async request to chansim api by sending process to background
		# and avoid session locks
		sim_request <- callr::r_bg(
			func = function(subject_pubkey, out_pubkey, in_pubkey, api_info) {
				req_body <- jsonlite::toJSON(
					list(subject_pubkey=subject_pubkey, out_pubkey=out_pubkey, in_pubkey=in_pubkey),
					auto_unbox=TRUE)
				if ("token" %in% names(api_info)) {
					api_request <- googleCloudRunner::cr_jwt_with_httr(
						httr::POST(url=api_info$url, body=req_body, encode="json"),
						api_info$token)
				} else {
					api_request <- httr::POST(url=api_info$url, body=req_body, encode="json")
				}
				return(httr::content(api_request))
			},
			args=list(subject_pubkey=subject, out_pubkey=out_node, in_pubkey=in_node, api_info=api_info),
			supervise=TRUE
		)
		return(sim_request)
	})
}

#' simulation type server
#'
#' server connected to \link{simTypeUI}
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @return returns reactive on input selection of payment or rebalance
#' @export
simTypeServer <- function(id) {
	moduleServer(id, function(input, output, session) {
		reactive(input$pay_or_rebal)
	})
}

#' histogram plot server
#'
#' computes a histogram output from the rebalance/payment simulation results,
#' connected to \link{histoTabUI}
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param plotId id specific to the plot output, e.g., path cost, max flow,
#' @param xlab label of the x-axis
#' @param xvar variable to plot as a histogram
#' @param sim_res simulation result reactive (as a tibble) with which we draw
#' the histogram
#' @return returns plot_ly histogram output
#' @export
histoPlotServer <- function(id, plotId, xlab, xvar, sim_res) {
	moduleServer(id, function(input, output, session) {
		output[[paste0("histo_", plotId, "_plot")]] <- renderPlotly({
			req(!is.null(sim_res()))
			plot_ly(sim_res() %>% filter(path_fee<10e3),
				x=~eval(parse(text=xvar))) %>%
					layout(
						xaxis=list(title=xlab),
						yaxis=list(title="Number of paths"))
		})
	})
}

#' scatterplot server
#'
#' computes a scatterplot output from the rebalance/payment simulation results,
#' connected to \link{scatterTabUI}
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param plotId id specific to the plot output, e.g., path cost vs maxflow, max
#' flow vs balance, etc.
#' @param xlab label of the x-axis
#' @param ylab label of the y-axis
#' @param xvar x-axis variable to plot
#' @param yvar y-axis variable to plot
#' @param sim_res simulation result reactive (as a tibble) with which we draw
#' the scatterplot
#' @return returns plot_ly scatterplot output
#' @export
scatterPlotServer <- function(id, plotId, xlab, ylab, xvar, yvar, sim_res) {
	moduleServer(id, function(input, output, session) {
		output[[paste0("scatter_", plotId, "_plot")]] <- renderPlotly({
			req(!is.null(sim_res))
			plot_ly(sim_res() %>% filter(path_fee<10e3),
				x=~eval(parse(text=xvar)), y=~eval(parse(text=yvar)),
				showlegend=TRUE, type='scatter',
				marker=list(
					color=~path_hops,
					size=15,
					colorscale="RdBu",
					colorbar=list(title='# of hops'),
					opacity=0.4)) %>%
					layout(
						xaxis=list(title=xlab),
						yaxis=list(title=ylab))
		})
	})
}

#' simulation result output server
#'
#' backend for generating simulation result summary statistics; blank by default
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param resId result ID, e.g., for mean, or median, etc.
#' @param tabId id of the metric on which we're fetching summary stats, e.g., path, flow, balance
#' @param xvar corresponding column name to tabId in the simulation result table
#' @param desc description of the metric
#' @param sim_res_reactive reactive simulation results
#' @return returns a valuebox output for given summary statistic for a particular metric
#' @export
simResultServer <- function(id, resId, tabId, xvar, desc, sim_res_reactive) {
	moduleServer(id, function(input, output, session) {
		sim_res <- reactiveVal(NULL)
		observeEvent(sim_res_reactive(), {
			sim_res(sim_res_reactive())
		})
		output[[resId]] <- renderValueBox({
			if (!is.null(sim_res())) {
				if (resId != "samples") {
					val <- eval(parse(text=paste0(resId, "(", parse(text=paste0("sim_res()$", xvar)), ")"))) %>%
						round(0) %>%
						format(scientific=FALSE) %>%
						prettyNum(big.mark=",")
				} else {
					val <- sim_res() %>% nrow
				}
			} else {
				val <- ""
			}
			valueBox(val, desc, color="blue")
		})
	})
}

#' main rebalance/payment simulation server
#'
#' backend handling of all inputs/outputs for rebalance simulation
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param api_info api url and auth token to POST rebalance simulation information
#' to local (if no auth token present) or remote backend
#' @export
rebalsimServer <- function(id, api_info) {
	moduleServer(id, function(input, output, session) {
		# build list of nodes to select from
		lapply(c("subject", "out_node", "in_node"), function(x) nodeListServer("node_select", listId=x))
		# reactive pubkey selections
		subject <- getNodePubkey("node_select", "subject")
		out_node <- getNodePubkey("node_select", "out_node")
		in_node <- getNodePubkey("node_select", "in_node")
		# simulation type selection; payment/rebalance
		output$sim_type_choice <- simTypeServer("sim_type")
		outputOptions(output, "sim_type_choice", suspendWhenHidden=FALSE)

		# start simulation reactive button
		sim_start_button <- startButtonServer("launch_sim", "launch_sim_button")

		# reactive output summary stats depending on active histogram tab
		histo_tab <- reactive({input$histo_tab})
		observe({
			req(input$histo_tab)
			histo_tab <- (str_split(input$histo_tab, "_") %>% unlist)[2]
			lapply(
				data.frame(
					stat=c(rep("samples", 3), rep("min", 3), rep("max", 3), rep("mean", 3), rep("median", 3), rep("sd", 3)),
					tab=rep(c("cost", "flow", "bal"), 6),
					xvar=c("path_fee", "max_path_flow", "known_1Mmin"),
					desc=c(
						"Number of paths sampled", "Number of paths sampled", "Number of paths sampled",
						"Lowest path cost (ppm)", "Lowest maximum liquidity flow (sat)", "Lowest percentage of channels with high liquidity availability",
						"Highest path cost (ppm)", "Highest maximum liquidity flow (sat)", "Highest percentage of channels with high liquidity availability",
						"Expected path cost (ppm)", "Expected maximum liquidity flow (sat)", "Expected percentage of channels with high liquidity availability",
						"Median path cost (ppm)", "Median path maximum liquidity flow (sat)", "Median percentage of channels with high liquidity availability",
						"Spread in path cost (ppm)", "Spread in path maximum liquidity flow (sat)", "Spread in percentage of channels with high liquidity availability")
				) %>%
				filter(tab==histo_tab) %>%
				t %>%
				as.data.frame,
				function(x)
					simResultServer(id="sim_result",
						resId=x[1],
						tabId=x[2],
						xvar=x[3],
						desc=x[4],
						sim_output
					)
			)
		})
		# build reactive histograms
		lapply(
			data.frame(
				plotId=c("cost", "flow", "bal"),
				xlab=c(
					"Total path cost (ppm)",
					"Path maximum liquidity flow (sat)",
					"Percentage of channels in path with at least 1M routable sats"),
				xvar=c("path_fee", "max_path_flow", "known_1Mmin")) %>% t %>% as.data.frame,
			function(x)
				histoPlotServer(id="histo_tab_selected", plotId=x[1], xvar=x[3], xlab=x[2], sim_res=sim_output)
		)
		# build reactive scatterplots
		lapply(
			data.frame(
				plotId=c("flowcost", "flowbal", "balcost"),
				xlab=c(
					"Path maximum liquidity flow (sat)",
					"Path maximum liquidity flow (sat)",
					"Path cost (ppm)"),
				ylab=c(
					"Path cost (ppm)",
					"% of path with known minimum of 1M routable sats",
					"% of path with known minimum of 1M routable sats"),
				xvar=c("max_path_flow", "max_path_flow", "path_fee"),
				yvar=c("path_fee", "known_1Mmin", "known_1Mmin")) %>% t %>% as.data.frame,
			function(x)
				scatterPlotServer(id="scatter_tab_selected", plotId=x[1], xlab=x[2], ylab=x[3], xvar=x[4], yvar=x[5], sim_output)
		)
		# start the simulation when the start button is selected
		sim_run <- eventReactive(sim_start_button(), {
			req(out_node() != "")
			req(in_node() != "")
			showModal(
				modalDialog(
					"Running simulation, please wait...",
					size='s', footer='It should take a few seconds. An invoice will be displayed when the results are ready.'
				)
			)
			simulationServer("launch_sim", subject, out_node, in_node, api_info)
		})
		# ping the background process and fetch results when done
		sim_result <- reactiveVal()
		observe({
			req(sim_run())
			if (isolate(sim_run()$is_alive())) {
				invalidateLater(1000)
			} else {
				sim_result(sim_run()$get_result())
				removeModal()
			}
		})
		# generate and manage invoice once simulation is done
		invoice <- invoiceHandlingServer(
			"rebalsim_inv",
			reactive_trigger=sim_result,
			inv_fetch_url=Sys.getenv("STORE_URL"),
			inv_amt=Sys.getenv("REBALSIM_MSAT"),
			inv_desc="rebalance simulator")
		# require that the invoice be paid to reactively show simulation results
		sim_output <- eventReactive(invoice(), {
			req(invoice() == "Paid")
			sim_result()
		})
	})

}

#' rebalance/payment simulation app standalone
#'
#' for dev/testing purposes
rebalsimApp <- function() {
	rebalsim_api_info <- if (Sys.getenv("LOCAL")) {
			list(url=Sys.getenv("REBALSIM_LOCAL_API_URL"))
		} else {
			get_api_info("rebalsim-api")
		}
	ui <- dashboardPage(
		dashboardHeader(title='Rebalance Simulator'),
		dashboardSidebar(),
		dashboardBody(rebalsimUI('x')),
		skin='yellow',
	)
	server <- function(input, output, session) {
		rebalsimServer('x', rebalsim_api_info)
	}
	shinyApp(ui, server)
  
}
