appLinkUI <- function(id, appId) {
	infoBoxOutput(NS(id, appId), width=6)
}

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
				lapply(c("byoc", "nodestats"), function(x) appLinkUI(NS(id, "local_apps"), x))
			),
			h3('Simulators'),
			fluidRow(
				lapply(c("chansim", "rebalsim"), function(x) appLinkUI(NS(id, "local_apps"), x))
			),
			h3('Community tools'),
			fluidRow(
				lapply(c("amboss", "lnrouter"), function(x) appLinkUI(NS(id, "external_apps"), x))
			)
		)
	)
}

externalAppServer <- function(id, boxId, appLink, boxTitle, boxSubtitle, linkIcon) {
	moduleServer(id, function(input, output, session) {
		output[[boxId]] <- renderInfoBox({
			ic <- apputils::icon(list(src=paste0("www/", linkIcon), width="90px"), lib="local")
			apputils::infoBox(tags$a(boxTitle, href=appLink, target="_blank"), subtitle=boxSubtitle, icon=ic, color=NULL)
		})
	})
}

localAppServer <- function(id, boxId, boxTitle, boxSubtitle, linkIcon) {
	moduleServer(id, function(input, output, session) {
		output[[boxId]] <- renderInfoBox({
			infoBox(a(boxTitle, onclick=paste0("openTab('", boxId, "')"), href="#"), subtitle=boxSubtitle, icon=icon(linkIcon), color='yellow')
		})
	})
}

dashboardServer <- function(id) {
	moduleServer(id, function(input, output, session) {
		lapply(
			data.frame(
				boxId=c("amboss", "lnrouter"),
				appLink=c("https://amboss.space", "https://lnrouter.app"),
				boxTitle=c("amboss", "lnrouter"),
				boxSubtitle=c("Explore a node's channels, fees and all the most useful summary stats at Amboss", "Explore channel balancedness with LnRouter to help identify well-managed nodes"),
				linkIcon=c("amboss.png", "lnrouter.png")
			) %>% t %>% as.data.frame,
			function(x) externalAppServer("external_apps", x[1], x[2], x[3], x[4], x[5])
		)
		lapply(
			data.frame(
				boxId=c("byoc", "nodestats", "rebalsim", "chansim"),
				boxTitle=c("Build your own chart", "Node stats", "Payment/rebalance simulator", "Channel simulator"),
				boxSubtitle=c(
					"Explore network-wide node data and gather insight on trends and correlations",
					"Explore your node's local network and gain insight on peers",
					"Estimate the potential cost of a payment or rebalance to gain insight on liquidity demand and bottlenecks",
					"Simulate opening or closing a channel on your node to measure influence in the network"),
				linkIcon=c("chart-bar", "project-diagram", "calculator", "edit")
			) %>% t %>% as.data.frame,
			function(x) localAppServer("local_apps", x[1], x[2], x[3], x[4])
		)
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
