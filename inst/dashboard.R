dashboardUI <- function(id) {
	addResourcePath('www', 'www')
	fluidRow(
		useShinyjs(),
		tags$style(HTML("
		  .box-header {
			padding: 0 10px 0 0;
		  }
		  .box-header h3 {
			width: 100%;
			padding: 10px;
		}")),
		tags$head(
			tags$style(HTML(".small-box {height: 80px}")),
			tags$style(HTML(".info-box-text {font-size: 20px}")),
			tags$style(HTML(".control-label {font-size: 18px}")),
			tags$style(HTML(".shiny-output-error { visibility: hidden; }")),
			tags$style(HTML(".shiny-output-error:before { visibility: hidden; }")),
			tags$style(
			  type = 'text/css',
			  '.modal-dialog { width: fit-content !important; }'
			),
		),
		tags$script(HTML("
		var openTab = function(tabName){
			$('a', $('.sidebar')).each(function() {
				if(this.getAttribute('data-value') == tabName) {
					this.click()
				};
			});
		}")),
		column(12,
			tags$style(".info-box-icon { background-color: #FFFFFF !important; color: #000000 !important; }"),
			tags$style(type = 'text/css', '.bg-NULL {background-color: #FFFFFF !important; }'),
			tags$style(type = 'text/css', '.bg- {background-color: #FFFFFF !important; }'),
			tags$style(HTML(".info-box-icon .img-local {position: absolute; top: auto; left: 15px; }")),
			h2("Develop your own data-driven Lightning Network insight", align='center'),
			h4("Discover network-wide statistics on nodes, interactively explore node local networks, measure the impact of opening or closing a channel, and identify potentially profitable paths in the network. Login to access additional features!", align='center'),
			h3('Visuals'),
			fluidRow(
				infoBoxOutput(NS(id, 'chartlink'), width=6),
				infoBoxOutput(NS(id, 'nodestatslink'), width=6)
			),
			h3('Simulators'),
			fluidRow(
				infoBoxOutput(NS(id, 'chansimlink'), width=6),
				infoBoxOutput(NS(id, 'rebalsimlink'), width=6),
			),
			h3('Community tools'),
			fluidRow(
				infoBoxOutput(NS(id, 'ambosslink'), width=6),
				infoBoxOutput(NS(id, 'lnrouterlink'), width=6),
			)
		)
	)
}

dashboardServer <- function(id) {
	moduleServer(id, function(input, output, session) {
		# dashboard rendering
		output$lnrouterlink <- renderInfoBox({
			ic <- apputils::icon(list(src="www/lnrouter.png", width="90px"), lib="local")
			apputils::infoBox(tags$a("lnrouter", href="https://lnrouter.app", target="_blank"), subtitle='Explore channel balancedness with LnRouter to help identify well-managed nodes', icon=ic, color=NULL)
		})
		output$ambosslink <- renderInfoBox({
			ic <- apputils::icon(list(src="www/amboss.png", width="90px"), lib="local")
			apputils::infoBox(a("amboss", href="https://amboss.space", target="_blank"), subtitle='Explore a node\'s channels, fees and all the most useful summary stats at Amboss', icon=ic, color='NULL')
		})
		output$chartlink <- renderInfoBox({
			infoBox(a('Build your own chart', onclick="openTab('chart')", href="#"), subtitle='Explore network-wide node data and gather insight on trends and correlations', icon=icon('chart-bar'), color='yellow')
		})
		output$nodestatslink <- renderInfoBox({
			infoBox(a('Node stats', onclick="openTab('nodestats')", href="#"), subtitle="Explore your node's local network and gain insight on peers", icon=icon('project-diagram', lib='font-awesome'), color='yellow')
		})
		output$rebalsimlink <- renderInfoBox({
			infoBox(a('Payment/rebalance simulator', onclick="openTab('rebalsim')", href="#"), subtitle="Estimate the potential cost of a payment or rebalance to gain insight on liquidity demand and bottlenecks", icon=icon('calculator', lib='font-awesome'), color='yellow')
		})
		output$chansimlink <- renderInfoBox({
			infoBox(a('Channel simulator', onclick="openTab('chansim')", href="#"), subtitle='Simulate opening or closing a channel on your node to measure influence in the network', icon=icon('edit'), color='yellow')
		})
	})
}

dashboardApp <- function() {
  
	ui <- dashboardPage(
		dashboardHeader(title='Dashboard'),
		dashboardSidebar(),
		dashboardBody(dashboardUI('x')),
		skin='yellow',
	)
	server <- function(input, output, session) {
		dashboardServer('x')
	}
	shinyApp(ui, server)
  
}
