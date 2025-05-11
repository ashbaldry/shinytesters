#' Use Shiny Testers
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
