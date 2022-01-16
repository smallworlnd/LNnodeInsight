title <- tags$p(tags$img(src='www/LNnodeinsight-tinylogonobkgrnd-white.svg', height='30', width='30'), 'LNnodeinsight', target="_blank")

header <- dashboardHeader(title=title,
#	tags$li(class="dropdown",tags$a("Telegram", href="https://t.me/LNnodeInsight", icon("telegram"), target="_blank")),
#	tags$li(class="dropdown",tags$a("Twitter", href="https://twitter.com/smallworlnd", icon("twitter"), target="_blank")),
#	tags$li(class="dropdown",tags$a("Email", href="mailto:smallworlnd@protonmail.com", icon("envelope"), target="_blank")),
#	tags$li(class="dropdown",tags$a("Node", href="https://amboss.space/node/0382b31dcff337311bf919411c5073c9c9a129890993f94f4a16eaaeffd91c7788", icon("bolt"), target="_blank")),
#	tags$li(class="dropdown",tags$a("Source", href="https://github.com/smallworlnd/LNnodeInsight", icon("github"), target="_blank"))
	#tags$li(class = "dropdown",
		#tags$li(class = "dropdown", actionLink("login", textOutput("logintext"))))
		#tags$li(class="dropdown", a('Login', onclick="openTab('account')", href=NULL, style="cursor: pointer;")),
	tags$li(
		class="dropdown",
		style="padding-top: 8px; padding-left: 8px;",
		uiOutput('login_nav')),
	tags$li(
		class="dropdown",
		style="padding-right: 8px; padding-top: 8px;",
		logoutUI("logout"),
	)
	#)
)

sidebar <- dashboardSidebar(
	tags$style(HTML(".sidebar-menu li a { font-size: 18px; }")),
	tags$style(HTML(".treeview-menu>li>a { font-size: 16px!important; }")),
	sidebarMenu(
		id='sidebar', icon=NULL,
		menuItem("Start", startExpanded=TRUE, tabName="home", icon=icon('arrow-right', lib='font-awesome'),
			menuSubItem("Dashboard", tabName="dashboard", icon=NULL),
			menuSubItem("Account", tabName="account", icon=NULL)),
		menuItem("Visuals", tabName="visuals", icon=icon('eye-open', lib='glyphicon'),
			menuSubItem("Build your own chart", tabName="chart", icon=NULL),
			menuSubItem("Node stats", tabName="nodestats", icon=NULL)),
		menuItem("Simulators", tabName="simulators", icon=icon('route'),
			menuItem("Rebalance simulator", tabName="rebalsim", icon=NULL),
			menuItem("Channel simulator", tabName="chansim", icon=NULL)),
		menuItemOutput("recOpt"),
		menuItem("FAQ", tabName="faq", icon=icon('question')),
		menuItem("Contact", tabName="contact", icon=icon('comment'),
			menuItem("Telegram", href="https://t.me/LNnodeInsight", icon=icon('telegram')),
			menuItem("Twitter", href="https://twitter.com/smallworlnd", icon=icon('twitter')),
			menuItem("Email", href="mailto:smallworlnd@protonmail.com", icon=icon('envelope')),
			menuItem("Node", href="https://amboss.space/node/0382b31dcff337311bf919411c5073c9c9a129890993f94f4a16eaaeffd91c7788", icon=icon('bolt')),
			menuItem("Source", href="https://github.com/smallworlnd/LNnodeInsight", icon=icon('github'))),
		menuItem("Support our work", tabName="support", icon=icon('heart'))
	))


dashboardbody <- dashboardBody(
	rclipboardSetup(),
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
	tabItems(
		tabItem(tabName='account',
			loginUI("login", additional_ui=a('Not sure how to sign a message?', onclick="openTab('faq')", href=NULL, style="cursor: pointer;")),
			uiOutput("account_page"),
		),
		tabItem(tabName='dashboard',
			tags$style(".info-box-icon { background-color: #FFFFFF !important; color: #000000 !important; }"),
			tags$style(type = 'text/css', '.bg-NULL {background-color: #FFFFFF !important; }'),
			tags$style(type = 'text/css', '.bg- {background-color: #FFFFFF !important; }'),
			tags$style(HTML(".info-box-icon .img-local {position: absolute; top: auto; left: 15px; }")),
			h2("Develop your own data-driven Lightning Network insight", align='center'),
			h4("Discover network-wide statistics on nodes, interactively explore node local networks, measure the impact of opening or closing a channel, and identify potentially profitable paths in the network", align='center'),
			h3('Visuals'),
			fluidRow(
				infoBoxOutput('chartlink', width=6),
				infoBoxOutput('nodestatslink', width=6)
			),
			h3('Simulators'),
			fluidRow(
				infoBoxOutput('chansimlink', width=6),
				infoBoxOutput('rebalsimlink', width=6),
			),
			h3('Community tools'),
			fluidRow(
				infoBoxOutput('ambosslink', width=6),
				infoBoxOutput('lnrouterlink', width=6),
			),
		),
		tabItem(tabName='chart',
			byocUI('byoc')
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
					),
#					fluidRow(
#						infoBoxOutput('lnrouterlink2', width=12)
#					)
				),
			),
		),
		tabItem(tabName='nodestats',
			fluidRow(
				column(8,
					fluidRow(
						box(selectizeInput(inputId="nodestats_subject", label='Enter pubkey/alias to view stats', choices=NULL, options=list(placeholder='Pubkey/alias')), background='yellow', width=12),
					)
				),
				column(4,
					tags$br(),
					tags$div("Also see node stats on"),
					uiOutput('ambossnodestats')
				)),
#				column(6,
#					uiOutput('table_vars')),
			h3('Current ranks'),
			fluidRow(
				valueBoxOutput('nodestats.cent.between', width=3) %>% bs_embed_tooltip(title="Betweenness centrality measures how many shortest paths a node sits in between any two other nodes. Higher ranking nodes tend to be in more shortest paths between other nodes and are thus more likely to be in a potential route.", placement='top'),
				valueBoxOutput('nodestats.cent.eigen', width=3) %>% bs_embed_tooltip(title="Eigenvector/hubness centrality is a node's influence in the network. Higher ranking nodes tend to have more channels, and are also connected to other high ranking nodes who themselves have many channels.", placement='top'),
				valueBoxOutput('nodestats.cent.close', width=3) %>% bs_embed_tooltip(title="Closeness/hopness centrality measures the distance from a node to any other in the network. Higher ranking nodes have to make fewer hops to reach any other node on the network.", placement='top'),
				valueBoxOutput('nodestats.nd', width=3) %>% bs_embed_tooltip(title="Rank according to the system designed by Lightning Labs.", placement='top')),
			fluidRow(
				valueBoxOutput('nodestats.cent.between.weight', width=3) %>% bs_embed_tooltip(title="Betweenness centrality but using channel capacities as weights.", placement='top'),
				valueBoxOutput('nodestats.cent.eigen.weight', width=3) %>% bs_embed_tooltip(title="Eigenvector/hubness centrality but using channel capacities as weights.", placement='top'),
				valueBoxOutput('nodestats.cent.close.weight', width=3) %>% bs_embed_tooltip(title="Closeness/hopness centrality but using channel capacities as weights.", placement='top'),
				valueBoxOutput('nodestats.bos', width=3) %>% bs_embed_tooltip(title="Rank according to the system designed by Alex Bosworth.", placement='top')),
			h3("Historical ranks"),
			conditionalPanel(
				condition="input.nodestats_subject != ''",
				conditionalPanel(
					condition="output.histranks_inv_settled==TRUE",
					withSpinner(plotlyOutput("rank_change", width=NULL)),
				),
				hr(),
				column(12, align='center',
					actionBttn(inputId='show_hist_ranks', label=paste('View historical ranks for', as.numeric(histranks_msat)/1e3, 'sats'), style='fill', color='success', block=FALSE)),
			),
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
			chansimUI('chansim')
		),
		tabItem(tabName='faq',
			box(p("Betweeness centrality measures the number of shortest paths that pass through a node. A higher number of shortest paths a node has to any two other node in the network, the more likely they will be included in a route depending on the liquidity balance of each channel in the path."), title="Betweenness centrality", width=NULL, collapsible=TRUE),
			box(p("Closeness/hopness centrality is a measure of how many hops it takes to reach any node on the network from a given node. The better the rank, the fewer the hops required to reach any and all nodes."), title="Closeness/hopness centrality", width=NULL, collapsible=TRUE),
			box(p("Eigenvector/hubness centrality measures influence of a given node in the network. Higher ranks imply a well-connected node that is linked to other well-connected nodes. A lower eigenvector centrality could also imply a new and/or underserved node in the network."), title="Eigenvector/hubness centrality", width=NULL, collapsible=TRUE),
			box(p("Maximum flow is the highest amount of sats that can theoretically be pushed through a path if liquidity were 100% outbound. In reality, outbound across a path is likely 50% or less."), title="Maximum liquidity flow", width=NULL, collapsible=TRUE),
			box(
				h4("lnd"), code('lncli signmessage "message to sign here"'),
				h4("c-lightning"), code('lightning-cli signmessage "message to sign here"'),
				h4("eclair"), code("eclair-cli signmessage --msg=$(echo -n 'message to sign here' | base64)"),
				h4("Ride The Lightning"), p("Navigate to Lightning > Sign/Verify > Copy"),
				h4("Thunderhub"), p("Navigate to Tools > Sign message > Copy"),
				title="Signing a message with your node's private keys", width=NULL, collapsible=TRUE),
		)
	))

ui <- function() {
	addResourcePath('www', 'www')
	tagList(
		dashboardPage(title='Bitcoin LN node insight', header, sidebar, dashboardbody, skin='yellow')
	)
}
