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
	useShinyjs(),
	tabItems(
		tabItem(tabName='account',
			loginUI("login", additional_ui=a('Not sure how to sign a message?', onclick="openTab('faq')", href=NULL, style="cursor: pointer;")),
			uiOutput("account_page"),
		),
		tabItem(tabName='dashboard',
			dashboardUI('dashboard')
		),
		tabItem(tabName='chart',
			byocUI('byoc')
		),
		tabItem(tabName='rebalsim',
			rebalsimUI('rebalsim')
		),
		tabItem(tabName='nodestats',
			nodeStatsUI('nodestats')
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
