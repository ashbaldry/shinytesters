#' Create Test Update Functions
#'
#' @description
#' Given a set of functions from an R package, create a set of mocked functions that
#' can be used as bindings to test UI updates within `testServer`
#'
#' @param fn_names,fn_name A character vector (string) of function names to create wrappers for
#' @param .package Character string of the package that `fn_names` exist in
#'
#' @return
#' TODO
#'
#' @examples
#' create_test_update_fns(
#'   c("updateSelectInput", "updateTextInput"),
#'   .package = "shiny"
#' )
#'
#' @rdname create_test_update_fn
#' @export
create_test_update_fns <- function(fn_names, .package = "shiny") {
  stats::setNames(
    lapply(fn_names, create_test_update_fn, .package = .package),
    fn_names
  )
}

#' @importFrom rlang :=
#' @rdname create_test_update_fn
#' @export
create_test_update_fn <- function(fn_name, .package = "shiny") {
  fn_expr <- get(fn_name, envir = asNamespace(.package))
  fn_body <- quote({
    fn_args <- rlang::fn_fmls_names()

    session_arg <- grep("session", fn_args, value = TRUE)
    if (length(session_arg) != 1L) {
      cli::cli_abort("Unable to determine session argument for {.fn fn_name}")
    }

    id_arg <- grep("[iI]d$", fn_args, value = TRUE)
    if (length(id_arg) != 1L) {
      cli::cli_abort("Unable to determine id argument for {.fn fn_name}")
    }

    id_value <- get(id_arg)

    value_arg <- grep("value|selected", fn_args, value = TRUE)
    value_value <- get(value_arg)
    update_input(id_value, value_value, session = session)

    choice_args <- grep("choice(Names|Values)", fn_args, value = TRUE)
    valid_choice_args <- length(choice_args) == 2L &&
      "choices" %in% fn_args &&
      is.null(get("choices")) &&
      !is.null(get("choiceNames"))
    if (valid_choice_args) {
      id_choice_value <- paste(id_value, "choices", sep = ".")
      session$setInputs(!!id_choice_value := stats::setNames(get("choiceValues"), get("choiceNames")))
    }

    other_args <- setdiff(fn_args, c(session_arg, id_arg, value_arg, choice_args))
    for (other_arg in other_args) {
      update_input(
        id = paste(id_value, other_arg, sep = "."),
        value = get(other_arg),
        session = session
      )
    }
  })

  rlang::new_function(rlang::fn_fmls(fn_expr), fn_body)
}

update_input <- function(id, value, session = shiny::getDefaultReactiveDomain()) {
  if (!is.null(value)) session$setInputs(!!id := value)
}
