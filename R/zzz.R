#' Splice Operator !!!
#'
#' @description
#' See \code{rlang::\link[rlang:splice-operator]{!!!}} for details.
#'
#' @usage NULL
#'
#' @return
#' The result of calling `do.call(fn_name, ...)`.
#'
#' @name splice-operator
#' @rdname splice-operator
#'
#' @examples
#' dfs <- list(mtcars, mtcars)
#' rlang::inject(rbind(!!!dfs))
#'
#' @keywords internal
#'
#' @importFrom rlang !!!
#' @export
NULL
