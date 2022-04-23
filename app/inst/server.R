server <- function(input, output, session) {
	users <- pool %>% tbl("users")
	credentials <- loginServer(
		id="login",
		data=users,
		pubkey_col="pubkey",
		cookie_logins=TRUE,
		reload_on_logout=TRUE,
		sessionid_col="sessionid",
		cookie_getter=get_sessions_from_db,
		cookie_setter=add_session_to_db,
		rest_url_base=Sys.getenv("VERIFY_MSG_URL"),
		rest_headers=rest_headers,
		rest_content=rest_content,
		log_out=reactive(logout_init())
	)
	chansim_api_info <- if (Sys.getenv("LOCAL")) {
			list(url=Sys.getenv("CHANSIM_LOCAL_API_URL"))
		} else {
			get_api_info("chansim-api")
		}
	rebalsim_api_info <- if (Sys.getenv("LOCAL")) {
			list(url=Sys.getenv("REBALSIM_LOCAL_API_URL"))
		} else {
			get_api_info("rebalsim-api")
		}
	logout_init <- logoutServer(
		id="logout",
		active=reactive(credentials()$user_auth)
	)
	output$login_nav <- renderUI({
		req(!credentials()$user_auth)
		actionBttn(inputId='login_nav', label='Login', style='fill', color='success', block=FALSE, size='sm')
	})
	onclick('login_nav', updateTabItems(session, "sidebar", "account"))

    runjs("
      $('.box').on('click', '.box-header h3', function() {
          $(this).closest('.box')
                 .find('[data-widget=collapse]')
                 .click();
      });")
	removeCssClass("ss-overlay", "ss-gray-out")
	observeEvent(input$sidebar, {
		req(input$sidebar == "support")
		showModal(
			modalDialog(
				title="Thanks for considering supporting us!",
				h4("Our Lightning Address:"),
				h2(a("smallworlnd@btcpay.lnnodeinsight.com")),
				size='l',
				easyClose=TRUE,
				footer=tagList(
					modalActionButton("donate_cancel", "Cancel")
				)
			)
		)
	})
	observeEvent(session$clientData$url_search, {
		query <- parseQueryString(session$clientData$url_search)
		if (length(query)>0) {
			pubkey <- str_split(names(query), '/')[[1]][2]
			pubkey_search <- node_ids[grepl(pubkey, node_ids)]
			if (length(pubkey_search) > 0) {
				updateTabItems(session, "sidebar", "nodestats")
				updateSelectizeInput(session, "nodestats_subject", choices=c("Pubkey/alias"="", node_ids), selected=pubkey_search, server=TRUE)
			}
		}
	})
	dashboardServer('dashboard')
	accountServer("account", credentials)
	byocServer('byoc', credentials)
	nodestatsServer('nodestats', credentials)
	rebalsimServer('rebalsim', rebalsim_api_info, credentials)
	chansimServer('chansim', chansim_api_info, credentials)
}
