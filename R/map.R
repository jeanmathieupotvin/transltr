#' Map function to values
#'
#' [map()], [vapply1i()], and [vapply1c()] respectively wrap [base::.mapply()]
#' and [base::vapply()]. Their purpose is to enforce specific arguments.
#'
#' @param fun a function to be applied on each element of `x`. See argument
#'   `FUN` of [base::mapply()] and/or [base::vapply()].
#'
#' @param moreArgs passed to argument `MoreArgs` of [base::mapply()].
#'
#' @param x passed to argument `X` of [base::vapply()].
#'
#' @param ... optional arguments to `fun`.
#'
#' @returns
#' * [map()] returns a list having the same length as values passed to `...`
#' * [vapply1l()] returns a logical vector having the same length as `x`.
#' * [vapply1i()] returns an integer vector having the same length as `x`.
#' * [vapply1c()] returns a character vector having the same length as `x`.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname utils-map
#'
#' @keywords internal
map <- function(fun, ..., moreArgs = list()) {
    return(.mapply(fun, list(...), moreArgs))
}

#' @rdname utils-map
vapply1l <- function(x, fun, ...) {
    return(vapply(x, fun, NA, ..., USE.NAMES = FALSE))
}

#' @rdname utils-map
vapply1i <- function(x, fun, ...) {
    return(vapply(x, fun, NA_integer_, ..., USE.NAMES = FALSE))
}

#' @rdname utils-map
vapply1c <- function(x, fun, ...) {
    return(vapply(x, fun, NA_character_, ..., USE.NAMES = FALSE))
}
