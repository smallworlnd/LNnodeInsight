source("inst/shiny-common.R", local=TRUE)

#' global variable useful for this app only
chart_vars <- c('Total capacity (BTC)'='tot.capacity',
	'Number of channels'='num.channels',
	'Average channel capacity (sat)'='avg.capacity',
	'Median channel capacity (sat)'='med.capacity',
	'Mean base fee (msat)'='mean.base.msat',
	'Median base fee (msat)'='median.base.msat',
	'Mean fee rate (ppm)'='mean.rate.ppm',
	'Median fee rate (ppm)'='median.rate.ppm',
	'Number of active channels'='act.channels',
	'Number of inactive channels'='inact.channels',
	'Betweenness centrality'='cent.between',
	'Eigenvector centrality'='cent.eigen',
	'Closeness centrality'='cent.close',
	'Terminal Web score'='tweb.score')

#' histogram UI element
#'
#' bundles selectors for various node properties and amboss communities
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param vars named vector of chart variables included in this file
#' @param communities_list vector of amboss communities
#' @return returns basic build-your-own histogram UI element
#' @export
histogramUI <- function(id, vars=chart_vars, communities_list=comms_list) {
	ns <- shiny::NS(id)
	tabPanel('Histogram', value='histo', id=ns('histo'), width=NULL,
		column(2,
			fluidRow(
				box(
					selectizeInput(inputId=ns('histo_var'), label='Choose a node variable', choices=c('', names(vars))),
					selectizeInput(inputId=ns('histo_comm'), label='Filter by Amboss community', choices=c('', communities_list)),
					checkboxGroupInput(inputId=ns('histo_log_scale'), label=NULL, c("Log-transform x-axis"="logx", "Log-transform y-axis"="logy"), selected="logy"),
					numericInput(inputId=ns("histo_min"), label="Set minimum value:", value=NULL),
					numericInput(inputId=ns("histo_max"), label="Set maximum value:", value=NULL),
				column(12, offset=3,
					actionBttn(inputId=ns('histo_clear'), label='Clear', style='fill', color='danger', block=FALSE)),
					width=NULL
				)
			),
		),
		column(10,
			fluidRow(
				box(withSpinner(plotlyOutput(ns('histo'), height="700px")), width=NULL),
			),
		)
	)
}

#' scatterplot UI element
#'
#' bundles selectors for various node properties and amboss communities
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param vars named vector of chart variables included in this file
#' @param communities_list vector of amboss communities
#' @return returns basic build-your-own scatterplot UI element
#' @export
scatterplotUI <- function(id, vars=chart_vars, communities_list=comms_list) {
	ns <- shiny::NS(id)
	tabPanel('Scatterplot', value='scatter', id=ns('scatter'), width=NULL,
		column(2,
			fluidRow(
				box(
					selectizeInput(inputId=ns('scatter_xvar'), label='Choose an X-axis variable', choices=c('', names(vars))),
					selectizeInput(inputId=ns('scatter_yvar'), label='Choose a Y-axis variable', choices=c('', names(vars))),
					shinyjs::hidden(
						selectizeInput(inputId=ns('scatter_comm'), label='Filter by Amboss community', choices=c('', communities_list))
					),
					checkboxGroupInput(inputId=ns('scatter_log_scale'), label=NULL, c("Log-transform x-axis"="logx", "Log-transform y-axis"="logy"), selected=c("logx", "logy")),
					numericInput(inputId=ns("scatter_xmin"), label="Set minimum X-axis value:", value=NULL),
					numericInput(inputId=ns("scatter_xmax"), label="Set maximum X-axis value:", value=NULL),
					numericInput(inputId=ns("scatter_ymin"), label="Set minimum Y-axis value:", value=NULL),
					numericInput(inputId=ns("scatter_ymax"), label="Set maximum Y-axis value:", value=NULL),
					column(12, offset=3,
						actionBttn(inputId=ns('scatter_clear'), label='Clear', style='fill', color='danger', block=FALSE)),
					width=NULL
				),
			),
		),
		column(10,
			fluidRow(
				box(withSpinner(plotlyOutput(ns('scatter'), height="700px")), width=NULL),
			)
		)
	)
}

#' histogram server module
#'
#' backend to build plot_ly histogram output depending on user input from
#' \link{histogramUI} and conditionally filter histogram data by community if
#' user is logged in
#'
#' TODO: separate out the updateSelectizeInput functions like with
#' \link{nodeListServer}
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param vars named vector of chart variables included in this file
#' @param communities_list vector of amboss communities
#' @param credentials reactive for checking if user is logged in to conditionally
#' show filter by community
#' @return returns plot_ly output of a histogram
#' @export
histogramServer <- function(id, credentials, db=pool, vars=chart_vars, communities_list=comms_list) {
	moduleServer(id, function(input, output, session) {
		shiny::observe({
			shinyjs::toggle('histo_comm', condition=credentials()$user_auth)
		})
		output$histo <- renderPlotly({
			req(input$histo_var != "")
			nd <- tbl(db, "nd") %>%
				filter(time==max(time)) %>%
				dplyr::select(pubkey, score) %>%
				rename('tweb.score'='score')
			dat <- tbl(db, "nodes_current") %>%
				left_join(., nd, by='pubkey')
			if (credentials()$user_auth && input$histo_comm != "") {
				pubkeys <- tbl(db, 'communities') %>%
					filter(community %in% !!input$histo_comm) %>%
					pull(pubkey)
				dat <- dat %>%
					filter(pubkey %in% !!pubkeys) %>%
					dplyr::select(local(chart_vars[input$histo_var] %>% as.vector)) %>%
					as_tibble
			} else {
				dat <- dat %>%
					dplyr::select(local(chart_vars[input$histo_var] %>% as.vector)) %>%
					as_tibble
			}
			xscale <- ifelse("logx" %in% input$histo_log_scale, "log", "linear")
			yscale <- ifelse("logy" %in% input$histo_log_scale, "log", "linear")
			if (vars[input$histo_var]=="tot.capacity") {
				dat <- dat %>% mutate(tot.capacity=tot.capacity/1e8)
			}
			if (!is.na(input$histo_min) && input$histo_min > 0) {
				dat <- dat %>%
					filter(eval(parse(text=vars[input$histo_var]))>=input$histo_min)
			}
			if (!is.na(input$histo_max) && input$histo_max > 0) {
				dat <- dat %>%
					filter(eval(parse(text=vars[input$histo_var]))<=input$histo_max)
			}
			plot_ly(dat %>% as_tibble,
				x=as.formula(paste0('~', vars[input$histo_var]))) %>%
					layout(
						xaxis=list(title=input$histo_var, type=xscale),
						yaxis=list(title="Number of nodes with", type=yscale)) %>%
					add_annotations(
						xref="paper", yref="paper",
						x=1, y=1, text="LNnodeinsight.com",
						font=list(size=16),
						showarrow=FALSE
					)
		}) %>% bindCache(input$histo_var, input$histo_min, input$histo_max, input$histo_comm)
		observeEvent(input$histo_clear, {
			updateSelectizeInput(session, inputId='histo_var', label='Choose a node variable', choices=c('', names(vars)))
			updateSelectizeInput(session, inputId='histo_comm', label='Filter by Amboss community', choices=c('', communities_list))
			updateNumericInput(session, "histo_min", label="Set minimum value:", value=NULL)
			updateNumericInput(session, "histo_max", label="Set maximum value:", value=NULL)
		})
	})
}

#' scatterplot server module
#'
#' backend to build plot_ly scatterplot output depending on user input from
#' \link{scatterplotUI} and conditionally filter scatterplot data by community if
#' user is logged in
#'
#' TODO: separate out the updateSelectizeInput functions like with
#' \link{nodeListServer}
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials reactive for checking if user is logged in to conditionally
#' @param vars named vector of chart variables included in this file
#' @param communities_list vector of amboss communities
#' show filter by community
#' @return returns plot_ly output of a scatterplot
#' @export
scatterplotServer <- function(id, credentials, db=pool, vars=chart_vars, communities_list=comms_list) {
	moduleServer(id, function(input, output, session) {
		shiny::observe({
			shinyjs::toggle('scatter_comm', condition=credentials()$user_auth)
		})
		output$scatter <- renderPlotly({
			req(input$scatter_xvar != "", input$scatter_yvar != "")
			nd <- tbl(db, "nd") %>%
				filter(time==max(time)) %>%
				dplyr::select(pubkey, score) %>%
				rename('tweb.score'='score')
			dat <- tbl(db, "nodes_current") %>%
				left_join(., nd, by='pubkey')
			if (credentials()$user_auth && input$scatter_comm != "") {
				print('comm')
				pubkeys <- db %>% tbl('communities') %>%
					filter(community %in% !!input$scatter_comm) %>%
					pull(pubkey)
				dat <- dat %>%
					filter(pubkey %in% !!pubkeys) %>%
					dplyr::select(
						alias,
						local(chart_vars[input$scatter_xvar] %>% as.vector),
						local(chart_vars[input$scatter_yvar] %>% as.vector)) %>%
					as_tibble
			} else {
				dat <- dat %>%
					dplyr::select(
						alias,
						local(chart_vars[input$scatter_xvar] %>% as.vector),
						local(chart_vars[input$scatter_yvar] %>% as.vector)) %>%
					as_tibble
			}
			xscale <- ifelse("logx" %in% input$scatter_log_scale, "log", "linear")
			yscale <- ifelse("logy" %in% input$scatter_log_scale, "log", "linear")
			if (vars[input$scatter_xvar]=="tot.capacity" || vars[input$scatter_yvar]=="tot.capacity") {
				dat <- dat %>% mutate(tot.capacity=tot.capacity/1e8)
			}
			if (!is.na(input$scatter_xmin) && input$scatter_xmin > 0) {
				dat <- dat %>%
					filter(eval(parse(text=vars[input$scatter_xvar]))>=input$scatter_xmin)
			}
			if (!is.na(input$scatter_xmax) && input$scatter_xmax > 0) {
				dat <- dat %>%
					filter(eval(parse(text=vars[input$scatter_xvar]))<=input$scatter_xmax)
			}
			if (!is.na(input$scatter_ymin) && input$scatter_ymin > 0) {
				dat <- dat %>%
					filter(eval(parse(text=vars[input$scatter_xvar]))>=input$scatter_ymin)
			}
			if (!is.na(input$scatter_ymax) && input$scatter_ymax > 0) {
				dat <- dat %>%
					filter(eval(parse(text=vars[input$scatter_xvar]))<=input$scatter_ymax)
			}
			plot_ly(dat,
				type='scatter', text=~alias,
				hovertemplate=paste0("%{text}\n", "%{x}, %{y}"),
				x=as.formula(paste0('~', vars[input$scatter_xvar])),
				y=as.formula(paste0('~', vars[input$scatter_yvar]))) %>%
					layout(
						xaxis=list(title=input$scatter_xvar, type=xscale),
						yaxis=list(title=input$scatter_yvar, type=yscale)) %>%
					add_annotations(
						xref="paper", yref="paper",
						x=1, y=1, text="LNnodeinsight.com",
						font=list(size=16),
						showarrow=FALSE
					)
		}) %>% bindCache(input$scatter_xvar, input$scatter_xmin, input$scatter_xmax, input$scatter_yvar, input$scatter_ymin, input$scatter_ymax, input$scatter_comm)
		observeEvent(input$scatter_clear, {
			updateSelectizeInput(session, inputId='scatter_xvar', label='Choose an X-axis variable', choices=c('', names(vars)))
			updateSelectizeInput(session, inputId='scatter_yvar', label='Choose a Y-axis variable', choices=c('', names(vars)))
			updateSelectizeInput(session, inputId='scatter_comm', label='Color by Amboss community', choices=c('', communities_list))
			updateNumericInput(session, "scatter_xmin", label="Set minimum X-axis value:", value=NULL)
			updateNumericInput(session, "scatter_xmax", label="Set maximum X-axis value:", value=NULL)
			updateNumericInput(session, "scatter_ymin", label="Set minimum Y-axis value:", value=NULL)
			updateNumericInput(session, "scatter_ymax", label="Set maximum Y-axis value:", value=NULL)
		})
	})
}

#' main layout UI for various app elements
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @return returns app UI element
#' @export
byocUI <- function(id) {
	tabBox(id='charting', side='left', selected='histo', width=NULL, height='800px',
		histogramUI(NS(id, 'ln_histo')),
		scatterplotUI(NS(id, 'ln_scatter'))
	)
}

#' main app server
#'
#' module running histogram and scatterplot outputs
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param credentials reactively show additional filters conditional on user
#' being logged in
#' @return returns backend for the app UI
#' @export
byocServer <- function(id, credentials) {
	moduleServer(id, function(input, output, session) {
		histogramServer('ln_histo', credentials)
		scatterplotServer('ln_scatter', credentials)
	})
}

#' byoc app standalone
#'
#' for dev/testing purposes
byocApp <- function() {
  
	ui <- dashboardPage(
		dashboardHeader(title='Build Your Own Chart'),
		dashboardSidebar(),
		dashboardBody(byocUI('x')),
		skin='yellow',
	)
	credentials <- reactiveValues(
		info=data.frame(pubkey=test_pubkey, foo="bar"),
		user_auth=TRUE)
	server <- function(input, output, session) {
		byocServer("x", reactive(credentials))
	}
	shinyApp(ui, server)
  
}
