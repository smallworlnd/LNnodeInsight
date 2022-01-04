chart_vars <- c('Total capacity (sat)'='tot.capacity',
	'Number of channels'='num.channels',
	'Average channel capacity (sat)'='avg.capacity',
	'Median channel capacity (sat)'='med.capacity',
	'Mean base fee (msat)'='mean.base.msat',
	'Median base fee (msat)'='median.base.msat',
	'Mean fee rate (ppm)'='mean.rate.ppm',
	'Median fee rate (ppm)'='median.rate.ppm',
	'Number of active channels'='act.channels',
	'Number of inactive channels'='inact.channels',
	'Approximate node age (days)'='age',
	'Betweenness centrality'='cent.between',
	'Eigenvector centrality'='cent.eigen',
	'Closeness centrality'='cent.close',
	'Terminal Web score'='tweb.score',
	'BOS score'='bos')

comms <- tbl(con, 'communities') %>% as_tibble
comms_list <- comms %>% pull(community) %>% unique

histogramUI <- function(id, vars=chart_vars, communities_list=comms_list) {
	ns <- shiny::NS(id)
	tabPanel('Histogram', value='histo', id=ns('histo'), width=NULL,
		column(2,
			fluidRow(
				box(width=NULL,
					selectizeInput(inputId=ns('histo_var'), label='Choose a node variable', choices=c('', names(vars))),
					selectizeInput(inputId=ns('histo_comm'), label='Filter by Amboss community', choices=c('', communities_list)),
				column(12, offset=3,
					actionBttn(inputId=ns('histo_clear'), label='Clear', style='fill', color='danger', block=FALSE)),
				)
			),
		),
		column(10,
			fluidRow(
				box(withSpinner(plotlyOutput(ns('histo'))), width=NULL),
			),
		)
	)
}

scatterplotUI <- function(id, vars=chart_vars, communities_list=comms_list) {
	ns <- shiny::NS(id)
	tabPanel('Scatterplot', value='scatter', id=ns('scatter'), width=NULL,
		column(2,
			fluidRow(
				box(width=NULL,
					selectizeInput(inputId=ns('scatter_xvar'), label='Choose an X-axis variable', choices=c('', names(vars))),
					selectizeInput(inputId=ns('scatter_yvar'), label='Choose a Y-axis variable', choices=c('', names(vars))),
					shinyjs::hidden(
						selectizeInput(inputId=ns('scatter_comm'), label='Color by Amboss community', choices=c('', communities_list))
					),
					column(12, offset=3,
						actionBttn(inputId=ns('scatter_clear'), label='Clear', style='fill', color='danger', block=FALSE)),
				),
			),
		),
		column(10,
			fluidRow(
				box(withSpinner(plotlyOutput(ns('scatter'))), width=NULL),
			)
		)
	)
}

histogramServer <- function(id, tblgraph=g, vars=chart_vars, communities_list=comms_list, communities_df=comms, logged_in=FALSE) {
	moduleServer(id, function(input, output, session) {
		shiny::observe({
			shinyjs::toggle('histo_comm', condition=logged_in())
		})
		output$histo <- renderPlotly({
			if (logged_in() && input$histo_comm != "") {
				pubkeys <- communities_df %>% filter(community %in% input$histo_comm) %>% pull(pubkey)
				dat <- tblgraph %>% as_tibble %>% filter(name %in% pubkeys)
			} else {
				dat <- tblgraph %>% as_tibble
			}
			plot_ly(dat,
				x=as.formula(paste0('~', vars[input$histo_var]))) %>%
					layout(
						xaxis=list(title=input$histo_var, type='linear'),
						yaxis=list(title="Number of nodes with", type='log')) %>%
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
		})
	})
}

scatterplotServer <- function(id, tblgraph=g, vars=chart_vars, communities_list=comms_list, communities_df=comms, logged_in=FALSE) {
	moduleServer(id, function(input, output, session) {
		shiny::observe({
			shinyjs::toggle('scatter_comm', condition=logged_in())
		})
		output$scatter <- renderPlotly({
			if (logged_in() && input$scatter_comm != "") {
				pubkeys <- communities_df %>% filter(community %in% input$scatter_comm) %>% pull(pubkey)
				dat <- g %>% as_tibble %>% mutate(Group=ifelse(name %in% pubkeys, input$scatter_comm, "Rest of the LN"))
			} else {
				dat <- tblgraph %>% as_tibble %>% mutate(Group="LN")
			}
			plot_ly(dat %>% as_tibble,
				type='scatter', split=~Group, text=~alias,
				hovertemplate=paste0("%{text}\n", "%{x}, %{y}"),
				x=as.formula(paste0('~', vars[input$scatter_xvar])),
				y=as.formula(paste0('~', vars[input$scatter_yvar]))) %>%
					layout(
						xaxis=list(title=input$scatter_xvar, type='log'),
						yaxis=list(title=input$scatter_yvar, type='log')) %>%
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
		})
	})
}

byocUI <- function(id) {
	tabBox(id='charting', side='left', selected='histo', width=NULL, height='500px',
		histogramUI('ln_histo'),
		scatterplotUI('ln_scatter')
	)
}

byocServer <- function(input, output, session, reactive_show) {
	histogramServer('ln_histo', logged_in=reactive_show)
	scatterplotServer('ln_scatter', logged_in=reactive_show)
}

byocDemo <- function() {
  
	ui <- fluidPage(byocUI("x"))
	server <- function(input, output, session) {
		byocServer("x", reactive_show=reactive(TRUE))
	}
	shinyApp(ui, server)
  
}
