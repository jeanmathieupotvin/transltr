#' Apply a function over recursive elements
#'
#' Convenient wrapper functions to [base::.mapply()] and [base::vapply()].
#' The latter has a predetermined and constant return value compared to its
#' \pkg{base} equivalent.
#'
#' @param fun Passed to argument `FUN` of [base::.mapply()] and [base::vapply()].
#'
#' @param x Passed to argument `X` of [base::vapply()].
#'
#' @param useNames Passed to argument `USE.NAMES` of [base::vapply()].
#'
#' @param ... Passed to [base::.mapply()] and [base::vapply()].
#'
#' @returns
#' [vapply1i()] returns an integer vector.
#'
#' [vapply1c()] returns a character vector.
#'
#' [map()] returns a list.
#'
#' All outputs always have a length equal to the length of `x`.
#'
#' @author Jean-Mathieu Potvin (<jm@@potvin.xyz>)
#'
#' @keywords internal
map <- function(fun, ..., moreArgs = list()) {
    return(.mapply(fun, list(...), moreArgs))
}

#' @rdname map
#' @keywords internal
vapply1i <- function(x, fun, ..., useNames = FALSE) {
    return(vapply(x, fun, NA_integer_, ..., USE.NAMES = useNames))
}

#' @rdname map
#' @keywords internal
vapply1c <- function(x, fun, ..., useNames = FALSE) {
    return(vapply(x, fun, NA_character_, ..., USE.NAMES = useNames))
}
