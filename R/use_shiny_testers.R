#' Use Shiny Testers
#'
#' @description
#' Enable `update` functions in the Shiny package to be mocked in tests.
#'
#' @param .env Environment that defines effect scope. For expert use only.
#'
#' @return
#' Implicit return of the updated functions in the Shiny package within
#' the specified environment.
#'
#' @examples
#' library(shiny)
#' library(testthat)
#'
#' example_server_fn <- function(input, output, session) {
#'   observeEvent(input$trigger, {
#'       updateTextInput(
#'       inputId = "result",
#'       label = "New Label",
#'       value = NULL,
#'       placeholder = "New placeholder"
#'     )
#'   })
#' }
#'
#' test_that("Check that text input gets updated", {
#'   use_shiny_testers()
#'
#'   shiny::testServer(
#'     app = example_server_fn,
#'     expr = {
#'       session$setInputs(result = "Example text")
#'       session$setInputs(trigger = 1L)
#'
#'       expect_identical(input$result, "Example text")
#'       expect_identical(input$result.label, "New Label")
#'       expect_identical(input$result.placeholder, "New placeholder")
#'     }
#'   )
#' })
#'
#' @export
use_shiny_testers <- function(.env = rlang::caller_env()) {
  shiny_update_fns <- setdiff(
    grep("^update", getNamespaceExports("shiny"), value = TRUE),
    "updateQueryString"
  )

  testthat::local_mocked_bindings(
    !!!create_test_update_fns(shiny_update_fns, .package = "shiny"),
    .package = "shiny",
    .env = .env
  )
}
