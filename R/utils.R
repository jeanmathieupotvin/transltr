#' Nullish coalescing operator
#'
#' The nullish coalescing (`%??%`) operator is a logical operator that
#' returns its right-hand side operand when its left-hand side operand
#' is null and otherwise returns its left-hand side operand.
#'
#' @param lhs Any \R object. Left-hand side operand.
#'
#' @param rhs Any \R object. Right-hand side operand.
#'
#' @returns Argument `rhs` if `lhs` is `NULL`, and `lhs` otherwise.
#'
#' @note
#' The actual description of the operator was taken from
#' [mdn web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Nullish_coalescing).
#'
#' @rdname nullish-coalescing-op
#' @family utility functions
#' @keywords internal
`%??%` <- function(lhs, rhs) {
    return(if (is.null(lhs)) rhs else lhs)
}

#' Safe apply wrappers
#'
#' These functions wrap a function of the [`*apply()`][base::lapply()]
#' family and enforce various values for convenience and additional
#' safety.
#'
#' @param x Passed as is to argument `X` of [`*apply()`][base::lapply()].
#'
#' @param fun Passed as is to argument `FUN` of [`*apply()`][base::lapply()].
#'
#' @param ... Further arguments passed as is to [`*apply()`][base::lapply()].
#'   It later passes them to `fun`.
#'
#' @returns
#' [vapply_1l()] and [vapply_1c()] respectively return a logical and a
#' character vector of the same length as `x`. Names are always discarded.
#'
#' @rdname apply-wrappers
#' @family utility functions
#' @keywords internal
vapply_1l <- function(x, fun, ...) {
    return(vapply(x, fun, NA, ..., USE.NAMES = FALSE))
}

#' @rdname apply-wrappers
#' @keywords internal
vapply_1c <- function(x, fun, ...) {
    return(vapply(x, fun, NA_character_, ..., USE.NAMES = FALSE))
}
