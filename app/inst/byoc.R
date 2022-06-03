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
	'Terminal Web score'='tweb.score',
	'BOS score'='bos')

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
				box(width=NULL,
					selectizeInput(inputId=ns('histo_var'), label='Choose a node variable', choices=c('', names(vars))),
					selectizeInput(inputId=ns('histo_comm'), label='Filter by Amboss community', choices=c('', communities_list)),
					checkboxGroupInput(inputId=ns('histo_log_scale'), label=NULL, c("Log-transform x-axis"="logx", "Log-transform y-axis"="logy"), selected="logy"),
					numericInput(inputId=ns("histo_min"), label="Set minimum value:", value=NULL),
					numericInput(inputId=ns("histo_max"), label="Set maximum value:", value=NULL),
				column(12, offset=3,
					actionBttn(inputId=ns('histo_clear'), label='Clear', style='fill', color='danger', block=FALSE)),
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
				box(width=NULL,
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
		min_max <- eventReactive(input$histo_var, {
			req(input$histo_var != "")
			chart_var <- chart_vars[input$histo_var] %>% as.vector
			min_max <- tbl(db, "nodes_current") %>%
				left_join(., dplyr::select(tbl(db, "bos"), pubkey, score), by="pubkey") %>%
				rename("bos"="score") %>%
				left_join(., dplyr::select(tbl(db, "nd"), pubkey, score), by="pubkey") %>%
				rename("tweb.score"="score") %>%
				dplyr::select(local(chart_var)) %>%
				summarise_all(c("min", "max")) %>%
				collect
			if (chart_var=="tot.capacity") {
				min_max <- min_max %>% mutate(min=min/1e8, max=max/1e8)
			} else {
				min_max
			}
		})
		observeEvent(min_max(), {
			updateNumericInput(session, "histo_min", label="Set minimum value:", value=min_max()$min)
			updateNumericInput(session, "histo_max", label="Set maximum value:", value=min_max()$max)
		})
		output$histo <- renderPlotly({
			req(min_max())
			nd <- tbl(db, "nd") %>%
				filter(time==max(time)) %>%
				dplyr::select(pubkey, score) %>%
				rename('tweb.score'='score')
			bos <- tbl(db, "bos") %>%
				filter(time==max(time)) %>%
				dplyr::select(pubkey, score) %>%
				rename('bos'='score')
			dat <- tbl(db, "nodes_current") %>%
				left_join(., nd, by='pubkey') %>%
				left_join(., bos, by='pubkey')
			if (credentials()$user_auth && input$histo_comm != "") {
				pubkeys <- db %>% tbl('communities') %>%
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
			dat <- dat %>%
				filter(eval(parse(text=vars[input$histo_var]))>=input$histo_min) %>%
				filter(eval(parse(text=vars[input$histo_var]))<=input$histo_max)
			plot_ly(dat,
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
		})
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
		x_min_max <- eventReactive(input$scatter_xvar, {
			req(input$scatter_xvar != "")
			xvar <- chart_vars[input$scatter_xvar] %>% as.vector
			min_max <- tbl(db, "nodes_current") %>%
				left_join(., dplyr::select(tbl(db, "bos"), pubkey, score), by="pubkey") %>%
				rename("bos"="score") %>%
				left_join(., dplyr::select(tbl(db, "nd"), pubkey, score), by="pubkey") %>%
				rename("tweb.score"="score") %>%
				dplyr::select(local(xvar)) %>%
				summarise_all(c("min", "max")) %>%
				collect
			if (xvar=="tot.capacity") {
				min_max <- min_max %>% mutate(min=min/1e8, max=max/1e8)
			} else {
				min_max
			}
		})
		y_min_max <- eventReactive(input$scatter_yvar, {
			req(input$scatter_yvar != "")
			yvar <- chart_vars[input$scatter_yvar] %>% as.vector
			min_max <- tbl(db, "nodes_current") %>%
				left_join(., dplyr::select(tbl(db, "bos"), pubkey, score), by="pubkey") %>%
				rename("bos"="score") %>%
				left_join(., dplyr::select(tbl(db, "nd"), pubkey, score), by="pubkey") %>%
				rename("tweb.score"="score") %>%
				dplyr::select(local(yvar)) %>%
				summarise_all(c("min", "max")) %>%
				collect
			if (yvar=="tot.capacity") {
				min_max <- min_max %>% mutate(min=min/1e8, max=max/1e8)
			} else {
				min_max
			}
		})
		observeEvent(x_min_max(), {
			updateNumericInput(session, "scatter_xmin", label="Set minimum X-axis value:", value=x_min_max()$min)
			updateNumericInput(session, "scatter_xmax", label="Set maximum X-axis value:", value=x_min_max()$max)
		})
		observeEvent(y_min_max(), {
			updateNumericInput(session, "scatter_ymin", label="Set minimum Y-axis value:", value=y_min_max()$min)
			updateNumericInput(session, "scatter_ymax", label="Set maximum Y-axis value:", value=y_min_max()$max)
		})
		output$scatter <- renderPlotly({
			req(x_min_max(), y_min_max())
			nd <- tbl(db, "nd") %>%
				filter(time==max(time)) %>%
				dplyr::select(pubkey, score) %>%
				rename('tweb.score'='score')
			bos <- tbl(db, "bos") %>%
				filter(time==max(time)) %>%
				dplyr::select(pubkey, score) %>%
				rename('bos'='score')
			dat <- tbl(db, "nodes_current") %>%
				left_join(., nd, by='pubkey') %>%
				left_join(., bos, by='pubkey')
			if (credentials()$user_auth && input$scatter_comm != "") {
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
			dat <- dat %>%
				filter(eval(parse(text=vars[input$scatter_xvar]))>=input$scatter_xmin) %>%
				filter(eval(parse(text=vars[input$scatter_xvar]))<=input$scatter_xmax) %>%
				filter(eval(parse(text=vars[input$scatter_yvar]))>=input$scatter_ymin) %>%
				filter(eval(parse(text=vars[input$scatter_yvar]))<=input$scatter_ymax)
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
		})
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
