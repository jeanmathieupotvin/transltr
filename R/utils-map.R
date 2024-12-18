#' Apply Wrappers
#'
#' These functions wrap a function of the [`apply()`][base::lapply()]
#' family, and enforce various values for convenience. Arguments are
#' passed *as is* to an [`apply()`][base::lapply()] function.
#'
#' @param x See argument `X` of [`vapply()`][base::lapply()].
#'
#' @param fun See argument `FUN` of [`vapply()`][base::lapply()], and
#'   [.mapply()].
#'
#' @param ... Further optional arguments passed to `fun`.
#'
#' @param more See argument `MoreArgs` of [.mapply()].
#'
#' @returns
#' [vapply_1l()],
#' [vapply_1l()], and
#' [vapply_1c()] respectively return a logical, an integer, and a character
#' vector having the same length as `x`. Names are always discarded.
#'
#' [map()] returns a list having the same length as the longest element passed
#' to  `...`.
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
map <- function(fun, ..., more = list()) {
    return(.mapply(fun, list(...), more))
}
