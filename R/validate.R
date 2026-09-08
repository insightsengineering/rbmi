#' Generic validation method
#'
#' This function is used to perform assertions that an object
#' conforms to its expected structure and no basic assumptions
#' have been violated. Will throw an error if checks do not pass.
#'
#' @param x object to be validated.
#' @param ... additional arguments to pass to the specific validation method.
#'
#' @return
#' Called for its side effect of validating `x`. Throws an error if any check
#' fails; otherwise returns invisibly (the exact value depends on the method,
#' typically `invisible(TRUE)` or `invisible(NULL)`).
#'
#' @keywords internal
#' @export
validate <- function(x, ...) {
    UseMethod("validate")
}
