#' Safe Apply Wrappers
#'
#' These functions wrap a function of the [`*apply()`][base::lapply()]
#' family and enforce various values for convenience and additional
#' safety.
#'
#' @param x Passed as is to argument `X` of [`*apply()`][base::lapply()].
#'
#' @param fun Passed as is to argument `FUN` of [`*apply()`][base::lapply()]
#'   or [.mapply()].
#'
#' @param ... Further arguments passed as is to [`*apply()`][base::lapply()]
#'   or [.mapply()]. It later passes them to `fun`.
#'
#' @returns
#' [vapply_1l()],
#' [vapply_1l()], and
#' [vapply_1c()] respectively return a logical, an integer, and a character
#' vector having the same length as `x`. Names are always discarded.
#'
#' [map()] returns a list having the same length as `x`.
#'
#' @rdname utils-map
#' @family utility functions
#' @keywords internal
vapply_1l <- function(x, fun, ...) {
    return(vapply(x, fun, NA, ..., USE.NAMES = FALSE))
}

#' @rdname utils-map
#' @keywords internal
vapply_1i <- function(x, fun, ...) {
    return(vapply(x, fun, NA_integer_, ..., USE.NAMES = FALSE))
}

#' @rdname utils-map
#' @keywords internal
vapply_1c <- function(x, fun, ...) {
    return(vapply(x, fun, NA_character_, ..., USE.NAMES = FALSE))
}

#' @rdname utils-map
#' @keywords internal
map <- function(fun, ..., moreArgs = list()) {
    return(.mapply(fun, list(...), moreArgs))
}
