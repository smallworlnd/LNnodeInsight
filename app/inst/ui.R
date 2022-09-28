title <- tags$p(tags$img(src='www/LNnodeinsight-tinylogonobkgrnd-white.svg', height='30', width='30'), 'LNnodeinsight', target="_blank")

header <- dashboardHeader(title=title,
	tags$li(
		class="dropdown",
		style="padding-top: 8px; padding-left: 8px;",
		uiOutput('is_premium_account')),
	tags$li(
		class="dropdown",
		style="padding-top: 8px; padding-left: 8px;",
		uiOutput('login_nav')),
	tags$li(
		class="dropdown",
		style="padding-right: 8px; padding-top: 8px;",
		logoutUI("logout"),
	)
)

sidebar <- dashboardSidebar(
	tags$style(HTML(".sidebar-menu li a { font-size: 18px; }")),
	tags$style(HTML(".treeview-menu>li>a { font-size: 16px!important; }")),
	sidebarMenu(
		id='sidebar', icon=NULL,
		menuItem("Start", startExpanded=TRUE, tabName="home", icon=icon('arrow-right', lib='font-awesome'),
			menuSubItem("Dashboard", tabName="dashboard", icon=NULL),
			menuSubItem("Account", tabName="account", icon=NULL),
			menuSubItem("Reports", tabName="reports", icon=NULL)
		),
		menuItem("Visuals", tabName="visuals", icon=icon('eye-open', lib='glyphicon'),
			menuSubItem("Build your own chart", tabName="byoc", icon=NULL),
			menuSubItem("Node stats", tabName="nodestats", icon=NULL)
		),
		menuItem("Simulators", tabName="simulators", icon=icon('route'),
			menuItem("Rebalance simulator", tabName="rebalsim", icon=NULL),
			menuItem("Channel simulator", tabName="chansim", icon=NULL),
			menuItem("Capacity-Fee simulator", tabName="capfeesim", icon=NULL)
		),
		menuItemOutput("recOpt"),
		menuItem("FAQ", tabName="faq", icon=icon('question')),
		menuItem("Contact", tabName="contact", icon=icon('comment'),
			menuItem("Telegram", href="https://t.me/LNnodeInsight", icon=icon('telegram')),
			menuItem("Twitter", href="https://twitter.com/smallworlnd", icon=icon('twitter')),
			menuItem("Email", href="mailto:smallworlnd@protonmail.com", icon=icon('envelope')),
			menuItem("Node", href="https://amboss.space/node/0382b31dcff337311bf919411c5073c9c9a129890993f94f4a16eaaeffd91c7788", icon=icon('bolt')),
			menuItem("Source", href="https://github.com/smallworlnd/LNnodeInsight", icon=icon('github')))
	))


dashboardbody <- dashboardBody(
	useShinyjs(),
	tabItems(
		tabItem(tabName='dashboard', dashboardUI('dashboard')),
		tabItem(tabName='account',
			loginUI("login", additional_ui=a('Not sure how to sign a message?', onclick="openTab('faq')", href=NULL, style="cursor: pointer;")),
			accountUI("account")
		),
		tabItem(tabName='reports', reportsUI("reports")),
		tabItem(tabName='byoc', byocUI('byoc')),
		tabItem(tabName='nodestats', nodestatsUI('nodestats')),
		tabItem(tabName='rebalsim', rebalsimUI('rebalsim')),
		tabItem(tabName='chansim', chansimUI('chansim')),
		tabItem(tabName='capfeesim', capfeesimUI('capfeesim')),
		tabItem(tabName='faq',
			between_box,
			close_box,
			eigen_box,
			maxflow_box,
			passive_box,
			active_box,
			sign_msg_box
		)
	))

ui <- function() {
	addResourcePath('www', 'www')
	tagList(
		dashboardPage(title='Bitcoin LN node insight', header, sidebar, dashboardbody, skin='yellow')
	)
}
