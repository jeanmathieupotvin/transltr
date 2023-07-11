#' Internal tools
#'
#' Internal utility functions wrapping and/or combining further functions.
#' They should not be used by end users. This documentation is intended for
#' developers.
#'
#' @param fmt passed to function [base::sprintf()]. It is concatenated into
#'   a single string before.
#'
#' @param fun passed to argument `FUN` of [base::.mapply()] or [base::vapply()].
#'
#' @param moreArgs passed to argument `MoreArgs` of [base::.mapply()].
#'
#' @param x passed to argument `X` of [base::vapply()].
#'
#' @param lhs,rhs any \R object.
#'
#' @param ... passed to further function(s).
#'
#' @details
#' [`%||%`][.stopf()] is the usual null coalescing operator. It returns its
#' right hand-side whenever its left hand-side is `NULL`. It is used to enforce
#' default values.
#'
#' ## Throw errors
#'
#' [stopf()] combines [base::sprintf()] and [base::stop()].
#'
#' ## Traverse functions
#'
#' [.map()], [.vapply1i()], and [.vapply1c()] respectively wrap
#' [base::.mapply()] and [base::vapply()]. Their purpose is to
#' enforce specific arguments.
#'
#' @returns
#' * [.stopf()] returns nothing. It is used for its side-effect.
#' * [.map()] returns a list having the same length as values passed to `...`
#' * [.vapply1i()] returns an integer vector having the same length as `x`.
#' * [.vapply1c()] returns a character vector having the same length as `x`.
#' * [`%||%`][.stopf()] returns `rhs` if and only if `lhs` is `NULL`. Else,
#'   `lhs` is returned.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname utils
#'
#' @keywords internal
.stopf <- function(fmt = character(), ...) {
    stop(sprintf(paste0(fmt, collapse = NULL), ...), call. = FALSE)
}

#' @rdname utils
#' @keywords internal
.map <- function(fun, ..., moreArgs = list()) {
    return(.mapply(fun, list(...), moreArgs))
}

#' @rdname utils
#' @keywords internal
.vapply1i <- function(x, fun, ...) {
    return(vapply(x, fun, NA_integer_, ..., USE.NAMES = FALSE))
}

#' @rdname utils
#' @keywords internal
.vapply1c <- function(x, fun, ...) {
    return(vapply(x, fun, NA_character_, ..., USE.NAMES = FALSE))
}

#' @rdname utils
#' @keywords internal
`%||%` <- function(lhs, rhs) {
    return(if (is.null(lhs)) rhs else lhs)
}
