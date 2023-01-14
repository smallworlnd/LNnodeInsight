server <- function(input, output, session) {
	apis <- c("chansim-api", "rebalsim-api", "capfeesim-api", "lnplus-swap-minmax")
	endpoints <- lapply(apis,
		function(x)
			if (Sys.getenv("LOCAL")) {
				list(url=Sys.getenv(x))
			} else {
				get_api_info(x)
			}
	)
	names(endpoints) <- apis

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
	logout_init <- logoutServer(
		id="logout",
		active=reactive(credentials()$user_auth)
	)
	output$login_nav <- renderUI({
		req(!credentials()$user_auth)
		actionBttn(inputId='login_nav', label='Login', style='fill', color='success', block=FALSE, size='sm')
	})
	onclick('login_nav', updateTabItems(session, "sidebar", "account"))
	account_is_premium <- premiumAccountReactive("prem_account", credentials, pool)
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

	observeEvent(input$sidebar, {
		if (input$sidebar == "dashboard") {
			dashboardServer('dashboard')
		}
		else if (input$sidebar == "account") {
			accountServer("account", credentials)
		}
		else if (input$sidebar == "reports") {
			reportServer('reports', credentials, endpoints$`lnplus-swap-minmax`)
		}
		else if (input$sidebar == "byoc") {
			byocServer('byoc', credentials)
		}
		else if (input$sidebar == "nodestats") {
			nodestatsServer('nodestats', credentials, url_pubkey_search=query_pubkey())
		}
		else if (input$sidebar == "capfeesim") {
			capfeesimServer('capfeesim', endpoints$`capfeesim-api`, credentials)
		}
		else if (input$sidebar == "rebalsim") {
			rebalsimServer('rebalsim', endpoints$`rebalsim-api`, credentials)
		}
		else if (input$sidebar == "chansim") {
			chansimServer('chansim', endpoints$`chansim-api`, credentials)
		}
	})

	session$allowReconnect(TRUE)
}
