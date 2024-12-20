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
#' @examples
#' # is.integer(), as.integer(), and as.character() are already
#' # vectorized by default. This is only for illustration purposes.
#' transltr:::vapply_1l(list(1L, 2L, 3L), is.integer)
#' transltr:::vapply_1i(list(1.1, 2.2, 3.3), as.integer)
#' transltr:::vapply_1c(list(1L, 2L, 3L), as.character)
#'
#' x <- transltr:::map(rep, c(1L, 2L, 3L), c(3L, 2L, 1L))
#' identical(x, list(c(1L, 1L, 1L), c(2L, 2L), 3L))
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
