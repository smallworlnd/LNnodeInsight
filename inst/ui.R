header <- dashboardHeader(title="LN Node Insight")

sidebar <- dashboardSidebar(
	tags$style(HTML(".sidebar-menu li a { font-size: 18px; }")),
	tags$style(HTML(".treeview-menu>li>a { font-size: 16px!important; }")),
	sidebarMenu(
		id='sidebar', icon=NULL,
		menuItem("Dashboard", tabName="dashboard", icon=icon('globe', lib='font-awesome')),
		menuItem("Applications", tabName="apps", icon=icon('th'),
			menuItem("Build your own chart", tabName="chart", icon=NULL),
			menuItem("Node peer network", tabName="peernet", icon=NULL),
			menuItem("Channel simulator", tabName="chansim", icon=NULL)),
		menuItem("FAQ", tabName="faq", icon=icon('question'))
	))


dashboardbody <- dashboardBody(
	useShinyjs(),
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
	tabItems(
		tabItem(tabName='dashboard',
			h2("Develop your own data-driven Lightning Network insight", align='center'),
			h4("Discover network-wide statistics on nodes, interactively explore node local networks, and measure the impact of opening or closing a channel", align='center'),
			hr(),
			fluidRow(
				infoBoxOutput('chartlink'),
				infoBoxOutput('peernetlink'),
				infoBoxOutput('chansimlink'),
				)),
		tabItem(tabName='chart',
			column(2,
				fluidRow(
					box(width=NULL,
						selectizeInput(inputId='chart_x', label='Choose an X-axis variable', choices=c('', names(chart_vars))),
						selectizeInput(inputId='chart_y', label='Choose a Y-axis variable', choices=c('', names(chart_vars))),
						awesomeCheckboxGroup(inputId='logscale', label='log-scale', choices=c('X-axis'='logx', 'Y-axis'='logy'), inline=TRUE),
						column(12, offset=4,
							actionBttn(inputId='chartclear', label='Clear', style='fill', color='danger', block=FALSE)),
					),
				),
			),
			column(10,
				fluidRow(
					box(withSpinner(plotlyOutput('userchart')), width=NULL, height="750px"),
				),
			)
		),
		tabItem(tabName='peernet',
			fluidRow(
				column(6,
					box(selectizeInput(inputId="view_node", label='Enter pubkey/alias to view local peer network', choices=NULL, options=list(placeholder="Pubkey/alias")), background='yellow', width=NULL)),
				column(6,
					uiOutput('table_vars'))),
			fluidRow(
				column(12,
					tabBox(id='peerinfotab', side='left', selected='forcenet', width=NULL,
						tabPanel('Peer network', forceNetworkOutput("net", height="750px"), value='forcenet', id='forcenet', width=NULL),
						tabPanel('Summary table', dataTableOutput('nodetable'), value='tablenet', id='tablenet', width=NULL))))),
		tabItem(tabName='chansim',
			fluidRow(
				column(6,
					box(selectizeInput(inputId="subject", label='#1: Enter your pubkey/alias', choices=NULL, options=list(placeholder="Pubkey or alias", maxItems=1)), background="yellow", width=NULL)),
				column(6,
					box(
						fluidRow(
							column(10,
								selectizeInput(inputId="target", label='#2: Enter pubkey/alias of a node to simulate add/remove channel', choices=NULL, options=list(placeholder="Pubkey or alias"))),
							column(2,
								prettyRadioButtons(inputId='add_or_del', label='', selected='add', choiceNames=c('Add', 'Remove'), choiceValues=c('add', 'del')))),
							column(12, offset=5,
								actionBttn(inputId='launch_sim', label='Start', style='fill', color='success', block=FALSE)),
						background='yellow', width=NULL))),
			fluidRow(
				box(title="Node centrality ranks", solidHeader=TRUE, collapsible=TRUE,
					valueBoxOutput('cent.between', width=4),
					valueBoxOutput('cent.eigen', width=4),
					valueBoxOutput('cent.close', width=4), width=NULL)),
			fluidRow(
				box(title="Network centralization scores", solidHeader=TRUE, collapsible=TRUE,
					valueBoxOutput('between.centralization', width=4),
					valueBoxOutput('eigen.centralization', width=4),
					valueBoxOutput('closeness.centralization', width=4), width=NULL))
		),
		tabItem(tabName='faq',
			box(p("Send me an ", a("email", href="mailto:smallworlnd@protonmail.com"), "or join the community on ", a("Telegram", href="https://t.me/LNnodeInsight"), "or connect to my node, ", a("LNnodeInsight.com [smallworlnd]", href="https://amboss.space/node/0382b31dcff337311bf919411c5073c9c9a129890993f94f4a16eaaeffd91c7788")), title="Contact?", width=NULL, collapsible=TRUE),
			box(p("See the code ", a("here", href="https://github.com/smallworlnd/LNnodeInsight"), "."), title="Open source?", width=NULL, collapsible=TRUE),
			box(p("Betweeness centrality measures the number of shortest paths that pass through a node. A higher number of shortest paths a node has (higher rank) to any other node in the network, the more likely they will be included in a calculated path, and the more likely they will be included in a route/forward depending on the liquidity balance of each channel in the path. Betweenness centralization is a measure for the whole network."), title="Betweenness centrality?", width=NULL, collapsible=TRUE),
			box(p("Closeness centrality is a measure of how many hops it takes to reach any node on the network from a given node. The better the rank, the fewer the hops required to reach any node. Closeness centralization is a measure for the whole network."), title="Closeness centrality?", width=NULL, collapsible=TRUE),
			box(p("Eigenvector centrality measures influence of a given node in the network. Higher scores imply a well-connected node that is linked to other well-connected nodes. A lower eigenvector centrality could also imply a new and/or underserved node in the network. Eigenvector centralization is computed for the whole network."), title="Eigenvector centrality?", width=NULL, collapsible=TRUE),
			box(p("The communities are inferred with the Louvain community detection algorithm. It detects clusters of nodes. It could be a useful metric to identify other nodes further away in the network."), title="Community?", width=NULL, collapsible=TRUE),
		)
	))

ui <- dashboardPage(header, sidebar, dashboardbody, skin='yellow')
