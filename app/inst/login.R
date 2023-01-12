
#' login UI module
#'
#' adapted from https://github.com/PaulC91/shinyauthr
#' Shiny UI Module for use with \link{loginServer}
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param title header title for the login panel
#' @param verify_title label for showing the verification message
#' @param signed_title label for the signed message input
#' @param login_title label for the login button
#' @param error_message message to display after failed login
#' @param additional_ui additional shiny UI element(s) to add below login button. Wrap multiple inside \code{shiny::tagList()}
#' @param cookie_expiry number of days to request browser to retain login cookie
#'
#' @return Shiny UI login panel with signed message input and login action button.
#' @example inst/shiny-examples/basic/app.R
#' @export
loginUI <- function(id,
                    title="Log in by signing the verification message below using your node",
                    verify_title="Verification message",
                    signed_title="Signed verification message",
                    login_title="Login",
                    error_message="Invalid signature!",
                    additional_ui=NULL,
                    cookie_expiry=7) {
  ns <- shiny::NS(id)

  verify_msg <- stri_rand_strings(1, 60, pattern="[A-Za-z0-9]")

  shinyjs::hidden(
    shiny::div(
      id=ns("panel"),
      style="width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      shiny::wellPanel(
	  	style="background-color: #f39c12;",
        shinyjs::useShinyjs(),
        jscookie_script(),
        shinyjs::extendShinyjs(text=js_cookie_to_r_code(ns("jscookie"), expire_days=cookie_expiry), functions=c("getcookie", "setcookie", "rmcookie")),
        shinyjs::extendShinyjs(text=js_return_click(ns("signed_msg"), ns("button")), functions=c()),
		shinyjs::extendShinyjs(text=detectProvider(ns("webln_detect")), functions=c()),
		shinyjs::extendShinyjs(text=requestProvider(ns("webln_enable")), functions=c("webln_enable")),
		shinyjs::extendShinyjs(text=requestMessageSig(ns("webln_signmsg")), functions=c("webln_signmessage")),
        shiny::tags$h2(title, class="text-center", style="padding-top: 0; color: white;"),
		br(),
		fluidRow(
			column(10,
				shiny::textInput(ns("verify_msg"), shiny::tagList(verify_title), verify_msg),
			),
			column(2,
				uiOutput(ns('clip')),
			),
			tags$style(type="text/css", "#copy_verify_msg {margin-top: 31px;}"),
			tags$style(type="text/css", "#login-verify_msg-label {color: white;}"),
		),
		textOutput(ns('timeleft')),
		tags$style(type="text/css", "#login-timeleft {color: white; font-size: 12px;}"),
        shiny::passwordInput(ns("signed_msg"), shiny::tagList(signed_title)),
		tags$style(type="text/css", "#login-signed_msg-label {color: white;}"),
        shiny::div(
          style="text-align: center;",
          shinyWidgets::actionBttn(inputId=ns("button"), label=login_title, style='fill', color='success')
        ),
        additional_ui,
        shinyjs::hidden(
          shiny::div(
            id=ns("error"),
            shiny::tags$p(
              error_message,
              style="color: red; font-weight: bold; padding-top: 5px;",
              class="text-center"
            )
          )
        ),
		hr(),
        shiny::div(
          style="text-align: center;",
          shinyWidgets::actionBttn(inputId=ns("webln_login_btn"), label=paste("WebLN", login_title), style='fill', color='success')
        ),
        shinyjs::hidden(
          shiny::div(
            id=ns("webln_error"),
            shiny::tags$p(
              "WebLN action canceled",
              style="color: red; font-weight: bold; padding-top: 5px;",
              class="text-center"
            )
          )
        ),
        shinyjs::hidden(
          shiny::div(
            id=ns("webln_sign_error"),
            shiny::tags$p(
              "Something went wrong with the signature verification",
              style="color: red; font-weight: bold; padding-top: 5px;",
              class="text-center"
            )
          )
        ),
      )
    )
  )
}

#' login server module
#'
#' Shiny authentication module for use with \link{loginUI}
#' 
#' This module uses shiny's new \link[shiny]{moduleServer} method as opposed to the \link[shiny]{callModule}
#' method used by the now deprecated \link{login} function and must be called differently in your app.
#' For details on how to migrate see the 'Migrating from callModule to moduleServer' section of 
#' \href{https://shiny.rstudio.com/articles/modules.html}{Modularizing Shiny app code}.
#'
#' @param id 	An ID string that corresponds with the ID used to call the module's UI function
#' @param data frame or tibble containing pubkeys other user data
#' @param signed_msg bare (unquoted) or quoted column name containing pubkeys
#' @param log_out [reactive] supply the returned reactive from \link{logoutServer} here to trigger a user logout
#' @param reload_on_logout should app force a session reload on logout?
#' @param cookie_logins enable automatic logins via browser cookies?
#' @param sessionid_col bare (unquoted) or quoted column name containing session ids
#' @param cookie_getter a function that returns a data.frame with at least two columns: user and session
#' @param cookie_setter a function with two parameters: user and session.  The function must save these to a database.
#'
#' @return The module will return a reactive 2 element list to your main application.
#'   First element \code{user_auth} is a boolean indicating whether there has been
#'   a successful login or not. Second element \code{info} will be the data frame provided
#'   to the function, filtered to the row matching the successfully logged in username.
#'   When \code{user_auth} is FALSE \code{info} is NULL.
#'
#' @importFrom rlang :=
#'
#' @example inst/shiny-examples/basic/app.R
#' @export
loginServer <- function(id, 
                        data,
						pubkey_col,
                        log_out=shiny::reactiveVal(),
                        reload_on_logout=FALSE,
                        cookie_logins=FALSE,
                        sessionid_col,
                        cookie_getter,
                        cookie_setter,
						rest_url_base,
						rest_headers,
						rest_content) {

  # if colnames are strings convert them to symbols
  try_class_uc <- try(class(pubkey_col), silent = TRUE)
  if (try_class_uc == "character") {
    pubkey_col <- rlang::sym(pubkey_col)
  }
  
  if (cookie_logins && (missing(cookie_getter) | missing(cookie_setter) | missing(sessionid_col))) {
    stop("if cookie_logins=TRUE, cookie_getter, cookie_setter and sessionid_col must be provided")
  } else {
    try_class_sc <- try(class(sessionid_col), silent = TRUE)
    if (try_class_sc == "character") {
      sessionid_col <- rlang::sym(sessionid_col)
    }
  }
  
  shiny::moduleServer(
    id,
    function(input, output, session) {

      timer <- reactiveVal(600)
      active <- reactiveVal(TRUE)
      output$timeleft <- renderText({
        paste("Message expires in", seconds_to_period(timer()))
      })
      observe({
        invalidateLater(1000)
        isolate({
          if(active())
          {
            timer(timer()-1)
            if(timer()<1)
            {
              active(FALSE)
            }
          }
        })
      })
	  verify_msg <- reactiveVal()
	  observe({
		invalidateLater(6e5)
		verify_msg(stri_rand_strings(1, 60, pattern="[A-Za-z0-9]"))
		updateTextInput(session, 'verify_msg', value=verify_msg())
	  })
		output$clip <- renderUI({
			rclipboard::rclipButton(inputId="copy_verify_msg", label=NULL, clipText=verify_msg(), icon=icon('clipboard'))
		})
      
      credentials <- shiny::reactiveValues(user_auth=FALSE, info=NULL, cookie_already_checked=FALSE)
      
      shiny::observeEvent(log_out(), {
        if (cookie_logins) {
          shinyjs::js$rmcookie()
        }

        if (reload_on_logout) {
          session$reload()
        } else {
          shiny::updateTextInput(session, "signed_msg", value="")
          verify_msg(stri_rand_strings(1, 60, pattern="[A-Za-z0-9]"))
          credentials$user_auth <- FALSE
          credentials$info <- NULL
        }
      })
      
      shiny::observe({
        if (cookie_logins) {
          if (credentials$user_auth) {
            shinyjs::hide(id="panel")
          } else if (credentials$cookie_already_checked) {
            shinyjs::show(id="panel")
          }
        } else {
          shinyjs::toggle(id="panel", condition=!credentials$user_auth)
        }
      })
      
      if (cookie_logins) {
        
        # possibility 1: login through a present valid cookie
        # first, check for a cookie once javascript is ready
        shiny::observeEvent(shiny::isTruthy(shinyjs::js$getcookie()), {
          shinyjs::js$getcookie()
        })
        # second, once cookie is found try to use it
        shiny::observeEvent(input$jscookie, {
          credentials$cookie_already_checked <- TRUE
          
          # if already logged in or cookie missing, ignore change in input$jscookie
          shiny::req(
            credentials$user_auth == FALSE,
            is.null(input$jscookie) == FALSE,
            nchar(input$jscookie) > 0
          )
          
          cookie_data <- dplyr::filter(cookie_getter(), {{sessionid_col}} == input$jscookie)

          if (nrow(cookie_data) != 1) {
            shinyjs::js$rmcookie()
          } else {
            # if valid cookie, we reset it to update expiry date
            .userid <- dplyr::pull(cookie_data, {{pubkey_col}})
            .sessionid <- randomString()
            
            shinyjs::js$setcookie(.sessionid)
            
            cookie_setter(.userid, .sessionid)
            
            cookie_data <- utils::head(dplyr::filter(cookie_getter(), {{sessionid_col}} == .sessionid, {{pubkey_col}} == .userid))
            
            credentials$user_auth <- TRUE
            credentials$info <- dplyr::bind_cols(
              dplyr::filter(data, {{pubkey_col}} == .userid) %>% as_tibble %>% arrange(desc(sub_date)) %>% distinct(pubkey, .keep_all=TRUE),
              dplyr::select(cookie_data, -{{pubkey_col}})
            )
          }
        })
        
      }
      
      # possibility 2: login through login button
      shiny::observeEvent(input$button, {
		
	  	# check if signed message yields a pubkey
		rest_content <- toJSON(list(msg=base64_enc(input$verify_msg), signature=input$signed_msg), auto_unbox=TRUE)
		signed_msg_output <- content(POST(url=rest_url_base, body=rest_content, config=rest_headers))
        
        # if signed message resolves to a valid pubkey, then credentials are valid
        if (signed_msg_output$valid) {
          credentials$user_auth <- TRUE
          credentials$info$pubkey <- signed_msg_output$pubkey
          
          if (cookie_logins) {
            .sessionid <- randomString()
            shinyjs::js$setcookie(.sessionid)
            cookie_setter(signed_msg_output$pubkey, .sessionid)
            cookie_data <- dplyr::filter(dplyr::select(cookie_getter(), -{{pubkey_col}}), {{sessionid_col}} == .sessionid)
            if (nrow(cookie_data) == 1) {
              credentials$info <- dplyr::bind_cols(credentials$info, cookie_data)
            }
          }
          
        } else { # if not valid temporarily show error message to user
          shinyjs::toggle(id="error", anim=TRUE, time=1, animType="fade")
          shinyjs::delay(5000, shinyjs::toggle(id="error", anim=TRUE, time=1, animType="fade"))
        }
      })

      # possibility #3: login with webln
		shiny::observeEvent(input$webln_login_btn, {
			if (input$webln_detect) {
				js$webln_enable()
			} else {
				showNotification("No WebLN provider detected. Try installing one like Alby.", type="error")
			}
		})
		webln_status <- shiny::eventReactive(input$webln_enable, {
			return(input$webln_enable)
		})
		observeEvent(c(webln_status(), input$webln_login_btn), {
			req(webln_status())
			js$webln_signmessage(input$verify_msg)
		})
		webln_msgsig <- shiny::eventReactive(input$webln_signmsg, {
			if ("catch" %in% names(input$webln_signmsg)) {
				shinyjs::toggle(id="webln_error", anim=TRUE, time=1, animType="fade")
				shinyjs::delay(5000, shinyjs::toggle(id="webln_error", anim=TRUE, time=1, animType="fade"))
			}
			return(input$webln_signmsg)
		})
		observeEvent(webln_msgsig(), {
			req("message" %in% names(webln_msgsig()), "signature" %in% names(webln_msgsig()))
			rest_content <- toJSON(list(msg=base64_enc(webln_msgsig()$message), signature=webln_msgsig()$signature), auto_unbox=TRUE)
			signed_msg_output <- content(POST(url=rest_url_base, body=rest_content, config=rest_headers))

			if (signed_msg_output$valid) {
			  credentials$user_auth <- TRUE
			  credentials$info$pubkey <- signed_msg_output$pubkey
			  
			  if (cookie_logins) {
				.sessionid <- randomString()
				shinyjs::js$setcookie(.sessionid)
				cookie_setter(signed_msg_output$pubkey, .sessionid)
				cookie_data <- dplyr::filter(dplyr::select(cookie_getter(), -{{pubkey_col}}), {{sessionid_col}} == .sessionid)
				if (nrow(cookie_data) == 1) {
				  credentials$info <- dplyr::bind_cols(credentials$info, cookie_data)
				}
			  }
			  
			} else { # if not valid temporarily show error message to user
			  shinyjs::toggle(id="webln_sign_error", anim=TRUE, time=1, animType="fade")
			  shinyjs::delay(5000, shinyjs::toggle(id="webln_sign_error", anim=TRUE, time=1, animType="fade"))
			}
		})

      # return reactive list containing auth boolean and user information
      shiny::reactive({
        shiny::reactiveValuesToList(credentials)
      })
      
    }
  )
}

