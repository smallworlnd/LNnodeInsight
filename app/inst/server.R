server <- function(input, output, session) {
	users <- pool %>% tbl("users") %>% as_tibble
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
	capfeesim_api_info <- if (Sys.getenv("LOCAL")) {
			list(url=Sys.getenv("CAPFEESIM_LOCAL_API_URL"))
		} else {
			get_api_info("capfeesim-api")
		}
	lnplus_swap_minmax_api_info <- if (Sys.getenv("LOCAL")) {
			list(url=Sys.getenv("LNPLUS_MINMAX_LOCAL_API_URL"))
		} else {
			get_api_info("lnplus-swap-minmax")
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
	account_is_premium <- premiumAccountReactive("prem_account", credentials, users)
	output$is_premium_account <- renderUI({
		req(credentials()$user_auth)
		req(account_is_premium() == "true")
		actionBttn(inputId='is_premium_account', label='Premium account', style='fill', color='success', block=FALSE, size='sm')
	})

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
	query_pubkey <- reactiveVal(character(0))
	observeEvent(session$clientData$url_search, {
		query <- parseQueryString(session$clientData$url_search)
		if (length(query)>0) {
			pubkey <- str_split(names(query), '/')[[1]][2]
			alias_pubkey <- node_ids[grepl(pubkey, node_ids)]
			if (length(alias_pubkey) > 0) {
				updateTabItems(session, "sidebar", "nodestats")
				query_pubkey(alias_pubkey)
			}
		}
	})
	dashboardServer('dashboard')
	accountServer("account", credentials)
	reportServer('reports', credentials, lnplus_swap_minmax_api_info)
	byocServer('byoc', credentials)
	nodestatsServer('nodestats', credentials, url_pubkey_search=query_pubkey())
	capfeesimServer('capfeesim', capfeesim_api_info, credentials)
	rebalsimServer('rebalsim', rebalsim_api_info, credentials)
	chansimServer('chansim', chansim_api_info, credentials)
}
