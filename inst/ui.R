title <- tags$p(tags$img(src='www/LNnodeinsight-tinylogonobkgrnd-white.svg', height='30', width='30'), 'LNnodeinsight', target="_blank")

header <- dashboardHeader(title=title,
	tags$li(class="dropdown",tags$a("Telegram", href="https://t.me/LNnodeInsight", icon("telegram"), target="_blank")),
	tags$li(class="dropdown",tags$a("Twitter", href="https://twitter.com/smallworlnd", icon("twitter"), target="_blank")),
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
		menuItem("Visuals", startExpanded=TRUE, tabName="visuals", icon=icon('eye-open', lib='glyphicon'),
			menuItem("Build your own chart", tabName="chart", icon=NULL),
			menuItem("Node stats", tabName="nodestats", icon=NULL)),
		menuItem("Simulators", startExpanded=TRUE, tabName="simulators", icon=icon('route'),
			menuItem("Rebalance simulator", tabName="rebalsim", icon=NULL, badgeLabel="New"),
			menuItem("Channel simulator", tabName="chansim", icon=NULL)),
		menuItem("FAQ", tabName="faq", icon=icon('question'))
	))


dashboardbody <- dashboardBody(
	rclipboardSetup(),
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
			h4("Discover network-wide statistics on nodes, interactively explore node local networks, measure the impact of opening or closing a channel, and identify potentially profitable paths in the network", align='center'),
			hr(),
			fluidRow(
				infoBoxOutput('chartlink', width=6),
				infoBoxOutput('nodestatslink', width=6),
				infoBoxOutput('chansimlink', width=6),
				infoBoxOutput('rebalsimlink', width=6),
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
		tabItem(tabName='rebalsim',
			fluidRow(
				column(8,
					fluidRow(
						box(
							prettyRadioButtons(inputId='pay_or_rebal', label='Simulate either a rebalance or a payment', selected=1, choiceNames=c('Rebalance', 'Payment'), choiceValues=c(1, 2), inline=TRUE),
							conditionalPanel(
								"input.pay_or_rebal == 1",
							# simulating a rebalance
								selectizeInput(inputId="rebalsim_subject", label='Step 1: enter a pubkey/alias on which to simulate a circular rebalance', choices=NULL, options=list(placeholder='Pubkey/alias')),
								selectizeInput(inputId="rebalsim_out_node", label=HTML('Step 2: enter/choose an outgoing node<br>*Can be a node with an existing channel to the one in Step 1, otherwise a channel will be simulated'), choices=NULL, options=list(placeholder='Pubkey/alias')),
								selectizeInput(inputId="rebalsim_in_node", label=HTML('Step 3: enter/select an incoming node<br>*Can be a node with an existing channel to the one in Step 1, otherwise a channel will be simulated'), choices=NULL, options=list(placeholder='Pubkey/alias')),
							),
							conditionalPanel(
								"input.pay_or_rebal == 2",
								# simulating a payment
								selectizeInput(inputId="paysim_out_node", label='Step 1: enter/choose a node to use as first hop for a payment', choices=NULL, options=list(placeholder='Pubkey/alias')),
								selectizeInput(inputId="paysim_in_node", label='Step 2: enter/select a node to use as payment destination', choices=NULL, options=list(placeholder='Pubkey/alias')),
							),
							column(12, align='center',
								actionBttn(inputId='launch_payrebalsim', label=paste('View simulation results for', as.numeric(rebalsim_msat)/1e3, 'sats'), style='fill', color='success', block=FALSE)),
							background='yellow', width=12,
						),
						tabBox(id='rebalsim_res_histo', side='left', width=12, selected='rebalsim_cost_histo',
							tabPanel(
								'Path cost histogram',
								withSpinner(plotlyOutput('rebal_cost_histo')), value='rebalsim_cost_histo',
								id='rebalsim_cost_histo_tab', width=NULL
							),
							tabPanel(
								'Path maximum liquidity flow histogram',
								withSpinner(plotlyOutput('rebal_flow_histo')), value='rebalsim_flow_histo',
								id='rebalsim_flow_histo_tab', width=NULL
							),
							tabPanel(
								'High liquidity availability histogram',
								withSpinner(plotlyOutput('rebal_bal_histo')), value='rebalsim_bal_histo',
								id='rebalsim_bal_histo_tab', width=NULL
							)
						),
						tabBox(id='rebalsim_res_scatter', side='left', width=12, selected='rebalsim_flowcost_scatter',
							tabPanel(
								'Maximum liquidity flow vs cost',
								withSpinner(plotlyOutput('rebal_flowcost_scatter')), value='rebalsim_flowcost_scatter',
								id='rebalsim_flowcost_scatter_tab', width=NULL
							),
							tabPanel(
								'Maximum liquidity flow vs high liquidity availability',
								withSpinner(plotlyOutput('rebal_flowbal_scatter')), value='rebalsim_flowbal_scatter',
								id='rebalsim_flowbal_scatter_tab', width=NULL
							),
							tabPanel(
								'High liquidity availability vs cost',
								withSpinner(plotlyOutput('rebal_balcost_scatter')), value='rebalsim_balcost_scatter',
								id='rebalsim_balcost_scatter_tab', width=NULL
							)
						)
					)
				),
				column(4,
					fluidRow(
						box(title="Summary stats", solidHeader=TRUE, collapsible=TRUE,
							valueBoxOutput('rebalsim.samples', width=12),
							valueBoxOutput('rebalsim.min', width=12),
							valueBoxOutput('rebalsim.max', width=12),
							valueBoxOutput('rebalsim.avg', width=12),
							valueBoxOutput('rebalsim.med', width=12),
							valueBoxOutput('rebalsim.sd', width=12),
							width=NULL
						),
					)
				),
			),
		),
		tabItem(tabName='nodestats',
			fluidRow(
				column(8,
					fluidRow(box(selectizeInput(inputId="nodestats_subject", label='Enter pubkey/alias to view stats', choices=NULL, options=list(placeholder='Pubkey/alias')), background='yellow', width=12)),
				)),
#				column(6,
#					uiOutput('table_vars')),
			h3('Ranks'),
			fluidRow(
				valueBoxOutput('nodestats.cent.between', width=3) %>% bs_embed_tooltip(title="", placement='top'),
				valueBoxOutput('nodestats.cent.eigen', width=3) %>% bs_embed_tooltip(title="", placement='top'),
				valueBoxOutput('nodestats.cent.close', width=3) %>% bs_embed_tooltip(title="", placement='top'),
				valueBoxOutput('nodestats.nd', width=3) %>% bs_embed_tooltip(title="", placement='top')),
			fluidRow(
				valueBoxOutput('nodestats.cent.between.weight', width=3) %>% bs_embed_tooltip(title="", placement='top'),
				valueBoxOutput('nodestats.cent.eigen.weight', width=3) %>% bs_embed_tooltip(title="", placement='top'),
				valueBoxOutput('nodestats.cent.close.weight', width=3) %>% bs_embed_tooltip(title="", placement='top'),
				valueBoxOutput('nodestats.bos', width=3) %>% bs_embed_tooltip(title="", placement='top')),
			h3(uiOutput('lncompare')),
			fluidRow(
				tabBox(id='nodestats_cap_change_tab', side='left', selected='node_cap_change', width=12,
					tabPanel('Total capacity', withSpinner(plotlyOutput("node_cap_change")), value='node_cap_change', id='node_cap_change', width=NULL),
					tabPanel('Channel capacity ', withSpinner(plotlyOutput("node_chansize_change")), value='node_chansize_change', id='node_chansize_change', width=NULL),
					tabPanel('Fees ', withSpinner(plotlyOutput("node_fee_change")), value='node_fee_change', id='node_fee_change', width=NULL),
				)
			),
			h3(uiOutput('peercompare')),
			fluidRow(
				tabBox(id='peer_chancap_change_tab', side='left', selected='fee_comp', width=12,
					tabPanel('Fee comparison', withSpinner(plotlyOutput("node_vs_peer_fees", height="750px")), value='fee_comp', id='fee_comp', width=NULL),
					tabPanel('Peers of peers in common', withSpinner(plotlyOutput("peer_overlap", height="750px")), value='peer_overlap', id='peer_overlap', width=NULL),
					tabPanel('Terminal Web stats', withSpinner(plotlyOutput("peer_ranks", height="750px")), value='peer_ranks', id='peer_ranks', width=NULL),
					tabPanel('Peer network', withSpinner(forceNetworkOutput("net", height="750px")), value='forcenet', id='forcenet', width=NULL),
				)
			),
		),
		tabItem(tabName='chansim',
			fluidRow(column(8,
				box(
					fluidRow(column(10, selectizeInput(inputId="chansim_subject", label='Step 1: enter your pubkey or alias', choices=NULL, options=list(placeholder='Pubkey/alias'))),
					column(2,
						tags$br(),
						tags$b("See node stats on"),
						uiOutput('ambosslink')
					)),
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
							sliderInput(inputId='tot.capacity.filt', label='Filter by range of total capacity (in bitcoin)', min=0.1, max=chansim_filter_parms$max.cap, step=0.1, value=c(0.1, chansim_filter_parms$max.cap), ticks=FALSE)),
						column(12,
							sliderInput(inputId='avg.capacity.filt', label='Filter by range average channel capacity (in bitcoin)', min=0.005, max=chansim_filter_parms$max.avg.capacity, step=0.01, value=c(0.005, chansim_filter_parms$max.avg.capacity), ticks=FALSE)),
						column(12,
							sliderInput(inputId='num.channels.filt', label='Filter by range of total channels', min=1, max=chansim_filter_parms$max.num.channels, step=1, value=c(1, chansim_filter_parms$max.num.channels), ticks=FALSE)),
						column(12,
							sliderInput(inputId='fee.rate.filt', label='Filter by range of median channel fee rates (ppm)', min=0, max=6000, step=1, value=c(0, 6000), ticks=FALSE)),
						column(12,
							sliderInput(inputId='age.filt', label='Filter by range of approximate node age (in days)', min=0, max=chansim_filter_parms$max.age, step=1, value=c(0, chansim_filter_parms$max.age), ticks=FALSE)),
						column(12,
							sliderInput(inputId='cent.between.rank.filt', label='Filter by range of betweenness centrality ranks', min=1, max=chansim_filter_parms$max.between, step=1, value=c(1, chansim_filter_parms$max.between), ticks=FALSE)),
						column(12,
							sliderInput(inputId='cent.close.rank.filt', label='Filter by range of closeness centrality ranks', min=1, max=chansim_filter_parms$max.close, step=1, value=c(1, chansim_filter_parms$max.close), ticks=FALSE)),
						column(12,
							sliderInput(inputId='cent.eigen.rank.filt', label='Filter by range of eigenvector centrality ranks', min=1, max=chansim_filter_parms$max.eigen, step=1, value=c(1, chansim_filter_parms$max.eigen), ticks=FALSE)),
						column(12,
							prettyRadioButtons(inputId='pubkey.or.alias', label='Show pubkey or alias or both in the drop-down menu', selected=3, choiceNames=c('Pubkey', 'Alias', 'Both'), choiceValues=c(1, 2, 3), inline=TRUE))),
						column(12, align='center',
							actionBttn(inputId='launch_sim', label='Start', style='fill', color='success', block=FALSE)),
					background='yellow', width=NULL),
					tabBox(id='chansim_peer_stats', side='left', selected='chansim_venn_tab', width=NULL,
						tabPanel(
							'Peer overlap',
							withSpinner(plotlyOutput('chansim_venn')), value='chansim_venn_tab',
							id='chansim_venn_tab', width=NULL
						) 
					),
				),
			column(4,
				fluidRow(box(title="Node centrality ranks", solidHeader=TRUE, collapsible=TRUE,
					valueBoxOutput('cent.between', width=12) %>% bs_embed_tooltip(title="Betweenness centrality measures how many shortest paths a node sits in between any two other nodes. Higher ranking nodes tend to be in more shortest paths between other nodes and are thus more likely to be in a potential route.", placement='top'),
					valueBoxOutput('cent.eigen', width=12) %>% bs_embed_tooltip(title="Eigenvector/hubness centrality is a node's influence in the network. Higher ranking nodes tend to have more channels, and are also connected to other high ranking nodes who themselves have many channels.", placement='top'),
					valueBoxOutput('cent.close', width=12) %>% bs_embed_tooltip(title="Closeness/hopness centrality measures the distance from a node to any other in the network. Higher ranking nodes have to make fewer hops to reach any other node on the network.", placement='top'), width=NULL)),
				fluidRow(box(title="Centralization scores of the network", solidHeader=TRUE, collapsible=TRUE,
					valueBoxOutput('between.centralization', width=12) %>% bs_embed_tooltip(title="Total betweenness score for the whole Lightning Network. Increase in your node's betweenness doesn't always mean increase in the network's betweenness.", placement='top'),
					valueBoxOutput('eigen.centralization', width=12) %>% bs_embed_tooltip(title="Total eigenvector/hubness score for the whole Lightning Network. Increase in your node's eigenvector centrality doesn't always mean increase in the network's eigenvector centrality.", placement='top'),
					valueBoxOutput('closeness.centralization', width=12) %>% bs_embed_tooltip(title="Total closeness/hopness score for the whole Lightning Network. Increase in your node's closeness doesn't always mean increase in the network's closeness centrality.", placement='top'), width=NULL))))
		),
		tabItem(tabName='faq',
			box(p(a("Email", href="mailto:smallworlnd@protonmail.com"), "/", a("Telegram", href="https://t.me/LNnodeInsight"), "/", a("Lightning node", href="https://amboss.space/node/0382b31dcff337311bf919411c5073c9c9a129890993f94f4a16eaaeffd91c7788")), title="Contact", width=NULL, collapsible=TRUE),
			box(p("See the code ", a("here", href="https://github.com/smallworlnd/LNnodeInsight"), "."), title="Source code", width=NULL, collapsible=TRUE),
			box(p("Betweeness centrality measures the number of shortest paths that pass through a node. A higher number of shortest paths a node has to any two other node in the network, the more likely they will be included in a route depending on the liquidity balance of each channel in the path."), title="Betweenness centrality", width=NULL, collapsible=TRUE),
			box(p("Closeness/hopness centrality is a measure of how many hops it takes to reach any node on the network from a given node. The better the rank, the fewer the hops required to reach any and all nodes."), title="Closeness/hopness centrality", width=NULL, collapsible=TRUE),
			box(p("Eigenvector/hubness centrality measures influence of a given node in the network. Higher ranks imply a well-connected node that is linked to other well-connected nodes. A lower eigenvector centrality could also imply a new and/or underserved node in the network."), title="Eigenvector/hubness centrality", width=NULL, collapsible=TRUE),
			box(p("Maximum flow is the highest amount of sats that can theoretically be pushed through a path if liquidity were 100% outbound. In reality, outbound across a path is likely 50% or less."), title="Maximum liquidity flow", width=NULL, collapsible=TRUE),
		)
	))

ui <- function() {
	addResourcePath('www', 'www')
	tagList(
		dashboardPage(title='Bitcoin LN node insight', header, sidebar, dashboardbody, skin='yellow')
	)
}
