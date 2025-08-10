#' Use Shiny Testers
#'
#' @description
#' Enable `update` functions in the Shiny or Shiny extension package to be mocked in tests.
#'
#' @param ... Arguments passed to \code{\link{create_test_update_fns}}
#' @param .package Character string of the package that the update functions exist in.
#' Default is `"shiny"`
#' @param .env Environment that defines effect scope. For expert use only.
#'
#' @return
#' Implicit return of the updated functions in the supplied package within
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
use_shiny_testers <- function(..., .package = "shiny", .env = rlang::caller_env()) {
  shiny_update_fns <- setdiff(
    grep("^update", getNamespaceExports(.package), value = TRUE),
    "updateQueryString"
  )

  testthat::local_mocked_bindings(
    !!!create_test_update_fns(shiny_update_fns, ..., .package = .package),
    .package = .package,
    .env = .env
  )
}

#' @param code Code to execute with specified bindings.
#'
#' @rdname use_shiny_testers
with_shiny_testers <- function(code, ..., .package = "shiny") {
  use_shiny_testers(..., .package = .package)
  code
}
