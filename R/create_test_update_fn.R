#' Create Test Update Functions
#'
#' @description
#' Given a set of functions from an R package, create a set of mocked functions that
#' can be used as bindings to test UI updates within `testServer`.
#'
#' @param fn_names A character vector (string) of function names to create wrappers for
#' @param id_arg A character string of the argument in `fn_names` that relates to the HTML ID argument.
#' Default is `"inputId"`
#' @param value_args A character vectors of the arguments in `fn_names` that relate to the input
#' value arguments.
#' Defaults are `"value"` and `"selected`.
#' @param range_value_args A character vectors of the arguments in `fn_names` that relate to the input
#' value arguments when multiple arguments can be used to update the input.
#' Defaults are `"start"` and `"end"`.
#' @param .package Character string of the package that `fn_names` exist in.
#' Default is `"shiny"`
#'
#' @return
#' A named list of function expressions, one for each function supplied in `fn_names`.
#'
#' @examples
#' create_test_update_fns(
#'   c("updateSelectInput", "updateTextInput"),
#'   .package = "shiny"
#' )
#'
#' @export
create_test_update_fns <- function(fn_names,
                                   id_arg = "inputId",
                                   value_args = c("value", "selected"),
                                   range_value_args = c("start", "end"),
                                   .package = "shiny") {
  stats::setNames(
    lapply(fn_names, create_test_update_fn, .package = .package),
    fn_names
  )
}

#' @importFrom rlang :=
create_test_update_fn <- function(fn_name,
                                  id_arg = "inputId",
                                  value_args = c("value", "selected"),
                                  range_value_args = c("start", "end"),
                                  .package = "shiny") {

  fn_expr <- get(fn_name, envir = asNamespace(.package))
  fn_body <- quote({
    fn_args <- rlang::fn_fmls_names()

    session_arg <- grep("session", fn_args, value = TRUE)
    if (length(session_arg) != 1L) {
      cli::cli_abort("Unable to determine session argument for {.fn fn_name}")
    }

    id_value <- get(id_arg)

    value_arg <- update_value(
      id = id_value,
      fn_args = fn_args,
      value_args = value_args,
      range_value_args = range_value_args,
      session = session
    )

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

#' Update the Input Value
#'
#' @description
#' Depending on the argument that is attached to the update function, update the relevant
#' input value with the specified value(s)
#'
#' @noRd
update_value <- function(id,
                         fn_args,
                         value_args = c("value", "selected"),
                         range_value_args = c("start", "end"),
                         session = shiny::getDefaultReactiveDomain()) {

  value_arg <- grep(paste(value_args, collapse = "|"), fn_args, value = TRUE)
  if (length(value_arg) == 1L) {
    value_value <- get(value_arg, envir = parent.frame())
    update_input(id, value_value, session = session)
    return(value_arg)
  }

  range_args <- grep(paste(range_value_args, collapse = "|"), fn_args, value = TRUE)
  if (length(range_args) == 2L) {
    new_values <- mget(range_args, envir = parent.frame())
    null_values <- vapply(new_values, is.null, logical(1L))
    if (all(null_values)) return(range_args)

    new_value_class <- class(new_values[!null_values][[1L]])
    if (any(null_values)) new_values[null_values] <- NA
    new_values <- structure(unlist(new_values, use.names = FALSE), class = new_value_class)

    curr_value <- session$input[[id]]
    if (!is.null(curr_value)) new_values[is.na(new_values)] <- curr_value[is.na(new_values)]
    update_input(id, new_values, session = session)
    return(range_args)
  }

  return(NULL)
}

update_input <- function(id, value, session = shiny::getDefaultReactiveDomain()) {
  if (!is.null(value)) session$setInputs(!!id := value)
}
