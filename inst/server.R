server <- function(input, output, session) {
	credentials <- loginServer(
		id="login",
		data=tbl(pool, "users"),
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
	output$account_page <- renderUI({
		req(credentials()$user_auth)
		acc <- users %>% filter(pubkey==!!pull(credentials()$info[1])) %>% as_tibble
		column(8, offset=2,
			box(title=NULL, background='yellow', width=12,
				p(style="text-align: left; font-size: 20px", strong(paste(acc$alias, "account page"))),
				hr(),
				fluidRow(
					column(6, align='left',
						p('Subscription')
					),
					column(6, align='right',
						p(acc$permissions)
					),
				)
			)
		)
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
	byocServer('byoc', reactive_show=reactive(credentials()$user_auth))
	nodestatsServer('nodestats')
	rebalsimServer('rebalsim')
	chansimServer('chansim', reactive(credentials()$user_auth))
}
