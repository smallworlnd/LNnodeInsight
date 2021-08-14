title <- tags$p(tags$img(src='www/LNnodeinsight-tinylogonobkgrnd-white.svg', height='30', width='30'), 'LNnodeinsight', target="_blank")

header <- dashboardHeader(title=title,
	tags$li(class="dropdown",tags$a("Telegram", href="https://t.me/LNnodeInsight", icon("telegram"), target="_blank")),
	tags$li(class="dropdown",tags$a("Email", href="mailto:smallworlnd@protonmail.com", icon("envelope"), target="_blank")),
	tags$li(class="dropdown",tags$a("Node", href="https://amboss.space/node/0382b31dcff337311bf919411c5073c9c9a129890993f94f4a16eaaeffd91c7788", icon("bolt"), target="_blank")),
	tags$li(class="dropdown",tags$a("Source", href="https://github.com/smallworlnd/LNnodeInsight", icon("github"), target="_blank"))
	)

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
		tabBox(id='charting', side='left', selected='histo', width=NULL, height="805px",
			tabPanel('Histogram', value='histo', id='histo', width=NULL,
				column(2,
					fluidRow(
					box(width=NULL,
							selectizeInput(inputId='histo_x', label='Choose a variable', choices=c('', names(chart_vars))),
					column(12, offset=3,
							actionBttn(inputId='histoclear', label='Clear', style='fill', color='danger', block=FALSE)),
					)
					),
				),
				column(10,
					fluidRow(
						box(withSpinner(plotlyOutput('histo')), width=NULL, height="750px"),
					),
				)
			),
			tabPanel('Scatterplot', value='scatter', id='scatter', width=NULL,
			column(2,
				fluidRow(
					box(width=NULL,
						selectizeInput(inputId='scatter_x', label='Choose an X-axis variable', choices=c('', names(chart_vars))),
						selectizeInput(inputId='scatter_y', label='Choose a Y-axis variable', choices=c('', names(chart_vars))),
						column(12, offset=3,
							actionBttn(inputId='scatterclear', label='Clear', style='fill', color='danger', block=FALSE)),
					),
				),
			),
			column(10,
				fluidRow(
					box(withSpinner(plotlyOutput('scatter')), width=NULL, height="750px"),
				),
			))),
		),
		tabItem(tabName='peernet',
			fluidRow(
				column(6,
					box(selectizeInput(inputId="view_node", label='Enter pubkey/alias to view local peer network', choices=NULL, options=list(placeholder='Pubkey/alias')), background='yellow', width=NULL)),
				column(6,
					uiOutput('table_vars'))),
			fluidRow(
				column(12,
					tabBox(id='peerinfotab', side='left', selected='forcenet', width=NULL,
						tabPanel('Peer network', forceNetworkOutput("net", height="750px"), value='forcenet', id='forcenet', width=NULL),
						tabPanel('Summary table', dataTableOutput('nodetable'), value='tablenet', id='tablenet', width=NULL))))),
		tabItem(tabName='chansim',
			fluidRow(column(8,
				box(
					fluidRow(column(10, selectizeInput(inputId="subject", label='Step 1: enter your pubkey or alias', choices=NULL, options=list(placeholder='Pubkey/alias'))),
					column(2,
						tags$br(),
						tags$b("See node stats on"),
						uiOutput('ambosslink')
					)), background="yellow", width="100%"),
				box(
					# first
					h4(p(strong('Step 2: enter or select pubkey/alias of up to 3 nodes with which to simulate adding or removing channels'))),
					fluidRow(
						column(10,
							selectizeInput(inputId="target", label=NULL, choices=NULL, options=list(placeholder='Pubkey/alias'))),
						column(2,
							prettyRadioButtons(inputId='add_or_del', label=NULL, selected='add', choiceNames=c('Add', 'Remove'), choiceValues=c('add', 'del')))),
					# second
					fluidRow(
						column(10,
							selectizeInput(inputId="target2", label=NULL, choices=NULL, options=list(placeholder='Pubkey/alias'))),
						column(2,
							prettyRadioButtons(inputId='add_or_del2', label=NULL, selected='add', choiceNames=c('Add', 'Remove'), choiceValues=c('add', 'del')))),
					# third
					fluidRow(
						column(10,
							selectizeInput(inputId="target3", label=NULL, choices=NULL, options=list(placeholder='Pubkey/alias'))),
						column(2,
							prettyRadioButtons(inputId='add_or_del3', label=NULL, selected='add', choiceNames=c('Add', 'Remove'), choiceValues=c('add', 'del')))),

						box(id="filt.box", title="Optional: apply filters to pubkey or alias menu choices", background="yellow", width=NULL, collapsible=TRUE, collapsed=TRUE, solidHeader=TRUE, status='primary',
						column(12,
							sliderInput(inputId='tot.capacity.filt', label='Filter by range of total capacity (in bitcoin)', min=0.1, max=200, step=0.1, value=c(0.1, 200), ticks=FALSE)),
						column(12,
							sliderInput(inputId='avg.capacity.filt', label='Filter by range average channel capacity (in bitcoin)', min=0.005, max=1.4, step=0.01, value=c(0.005, 1.4), ticks=FALSE)),
						column(12,
							sliderInput(inputId='num.channels.filt', label='Filter by range of total channels', min=1, max=2500, step=1, value=c(1, 2500), ticks=FALSE)),
						column(12,
							sliderInput(inputId='fee.rate.filt', label='Filter by range of median channel fee rates (ppm)', min=0, max=6000, step=1, value=c(0, 6000), ticks=FALSE)),
						column(12,
							sliderInput(inputId='age.filt', label='Filter by range of approximate node age (in days)', min=0, max=1300, step=1, value=c(0, 1300), ticks=FALSE)),
						column(12,
							sliderInput(inputId='cent.between.rank.filt', label='Filter by range of betweenness centrality ranks', min=1, max=11000, step=1, value=c(1, 11000), ticks=FALSE)),
						column(12,
							sliderInput(inputId='cent.close.rank.filt', label='Filter by range of closeness centrality ranks', min=1, max=11000, step=1, value=c(1, 11000), ticks=FALSE)),
						column(12,
							sliderInput(inputId='cent.eigen.rank.filt', label='Filter by range of eigenvector centrality ranks', min=1, max=11000, step=1, value=c(1, 11000), ticks=FALSE)),
						column(12,
							selectizeInput(inputId='community.filt', label='Filter by one or more communities', choices=g %>% as_tibble %>% select(community) %>% unique %>% pull %>% sort, multiple=TRUE, options=list(placeholder="Select community number"))),
						column(12,
							prettyRadioButtons(inputId='pubkey.or.alias', label='Show pubkey or alias or both in the drop-down menu', selected=3, choiceNames=c('Pubkey', 'Alias', 'Both'), choiceValues=c(1, 2, 3), inline=TRUE))),
						column(12, offset=5,
							actionBttn(inputId='launch_sim', label='Start', style='fill', color='success', block=FALSE)),
					background='yellow', width=NULL)),
			column(4,
				fluidRow(box(title="Node centrality ranks", solidHeader=TRUE, collapsible=TRUE,
					valueBoxOutput('cent.between', width=12) %>% bs_embed_tooltip(title="Betweenness centrality measures how many shortest paths a node sits in between any two other nodes. Higher ranking nodes tend to be in more shortest paths between other nodes and are thus more likely to be in a potential route.", placement='top'),
					valueBoxOutput('cent.eigen', width=12) %>% bs_embed_tooltip(title="Eigenvector centrality is a node's influence in the network. Higher ranking nodes tend to have more channels, and are also connected to other high ranking nodes who themselves have many channels.", placement='top'),
					valueBoxOutput('cent.close', width=12) %>% bs_embed_tooltip(title="Closeness centrality measures the distance from a node to any other in the network. Higher ranking nodes have to make fewer hops to reach any other node on the network.", placement='top'), width=NULL)),
				fluidRow(box(title="Network centralization scores", solidHeader=TRUE, collapsible=TRUE,
					valueBoxOutput('between.centralization', width=12) %>% bs_embed_tooltip(title="Network-wide index for betweenness.", placement='top'),
					valueBoxOutput('eigen.centralization', width=12) %>% bs_embed_tooltip(title="Network-wide index for eigenvector centrality.", placement='top'),
					valueBoxOutput('closeness.centralization', width=12) %>% bs_embed_tooltip(title="Network-wide index for closeness.", placement='top'), width=NULL))))
		),
		tabItem(tabName='faq',
			box(p(a("Email", href="mailto:smallworlnd@protonmail.com"), "/", a("Telegram", href="https://t.me/LNnodeInsight"), "/", a("Lightning node", href="https://amboss.space/node/0382b31dcff337311bf919411c5073c9c9a129890993f94f4a16eaaeffd91c7788")), title="Contact", width=NULL, collapsible=TRUE),
			box(p("See the code ", a("here", href="https://github.com/smallworlnd/LNnodeInsight"), "."), title="Source code", width=NULL, collapsible=TRUE),
			box(p("Betweeness centrality measures the number of shortest paths that pass through a node. A higher number of shortest paths a node has to any two other node in the network, the more likely they will be included in a route depending on the liquidity balance of each channel in the path."), title="Betweenness centrality", width=NULL, collapsible=TRUE),
			box(p("Closeness centrality is a measure of how many hops it takes to reach any node on the network from a given node. The better the rank, the fewer the hops required to reach any and all nodes."), title="Closeness centrality", width=NULL, collapsible=TRUE),
			box(p("Eigenvector centrality measures influence of a given node in the network. Higher ranks imply a well-connected node that is linked to other well-connected nodes. A lower eigenvector centrality could also imply a new and/or underserved node in the network."), title="Eigenvector centrality", width=NULL, collapsible=TRUE),
			box(p("The communities are inferred with the Louvain algorithm. It detects clusters of nodes. It could be a useful metric to identify groups of nodes further away from a given node in the network."), title="Community", width=NULL, collapsible=TRUE),
		)
	))

ui <- function() {
	addResourcePath('www', 'www')
	tagList(
		dashboardPage(title='Bitcoin LN node insight', header, sidebar, dashboardbody, skin='yellow')
	)
}
