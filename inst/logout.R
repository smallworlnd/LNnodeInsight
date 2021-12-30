# code adapted from https://github.com/PaulC91/shinyauthr
#' logout UI module
#'
#' Shiny UI Module for use with \link{logoutServer}
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param label label for the logout button
#' @param icon An optional \code{\link[shiny]{icon}} to appear on the button.
#' @param class bootstrap class for the logout button
#' @param style css styling for the logout button
#'
#' @return Shiny UI action button
#' @example inst/shiny-examples/basic/app.R
#' @export
logoutUI <- function(id, label="Logout", style='fill', color='danger', size='sm', block=FALSE) {
  ns <- shiny::NS(id)

  shinyjs::hidden(
    shinyWidgets::actionBttn(ns("button"), label=label, style=style, color=color, size=size, block=block)
  )
}

#' logout server module
#'
#' Shiny authentication module for use with \link{logoutUI}
#' 
#' This module uses shiny's new \link[shiny]{moduleServer} method as opposed to the \link[shiny]{callModule}
#' method used by the now deprecated \link{login} function and must be called differently in your app.
#' For details on how to migrate see the 'Migrating from callModule to moduleServer' section of 
#' \href{https://shiny.rstudio.com/articles/modules.html}{Modularizing Shiny app code}.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param active \code{reactive} supply the returned \code{user_auth} boolean reactive from \link{loginServer}
#'   here to hide/show the logout button
#' @param ... arguments passed to \link[shinyjs]{toggle}
#'
#' @return Reactive boolean, to be supplied as the \code{log_out} argument of the
#'   \link{loginServer} module to trigger the logout process
#'
#' @example inst/shiny-examples/basic/app.R
#' @export
logoutServer <- function(id, active, ...) {
  shiny::moduleServer(
    id,
    function (input, output, session) {
      shiny::observe({
        shinyjs::toggle(id="button", condition=active(), ...)
      })
      
      # return reactive logout button tracker
      shiny::reactive({
        input$button
      })
    }
  )
}
