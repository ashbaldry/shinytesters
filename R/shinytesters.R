#' Use Shiny Testers
#'
#' @export
use_shiny_testers <- function(.env = rlang::caller_env()) {
  shiny_update_fns <- grep("^update", getNamespaceExports("shiny"), value = TRUE)
  shiny_update_fns <- setdiff(shiny_update_fns, "updateQueryString")
  names(shiny_update_fns) <- shiny_update_fns

  testthat::local_mocked_bindings(
    !!!lapply(shiny_update_fns, create_shiny_tester_fn),
    .package = "shiny",
    .env = .env
  )
}

#' @export
create_shiny_tester_fn <- function(fn_name, package = "shiny") {
  fn_expr <- get(fn_name, envir = asNamespace(package))
  fn_body <- quote({
    fn_args <- rlang::fn_fmls_names()

    session_arg <- grep("session", fn_args, value = TRUE)
    if (length(session_arg) != 1L) {
      cli::cli_abort("Unable to determine session argument for {.fn fn_name}")
    }

    id_arg <- grep("[iI]d", fn_args, value = TRUE)
    if (length(id_arg) != 1L) {
      cli::cli_abort("Unable to determine id argument for {.fn fn_name}")
    }

    id_value <- get(id_arg)

    value_arg <- grep("value|selected", fn_args, value = TRUE)
    value_value <- get(value_arg)
    if (!is.null(value_value)) session$setInputs(!!id_value := value_value)

    other_args <- setdiff(fn_args, c(session_arg, id_arg, value_arg))
    for (other_arg in other_args) {
      id_concat_value <- paste(id_value, other_arg, sep = ".")
      other_value <- get(other_arg)

      if (!is.null(other_value)) session$setInputs(!!id_concat_value := other_value)
    }
  })

  rlang::new_function(rlang::fn_fmls(fn_expr), fn_body)
}
