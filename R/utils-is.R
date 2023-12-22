#' Test for specific shapes and values
#'
#' Check if a value has a specific shape ([type][base::typeof()],
#' [length][length()]) and/or value ([NAs][base::NA], boundaries, etc.).
#'
#' @param x any \R object to be tested.
#'
#' @param min a single numeric value. Lower inclusive boundary for `x`. This
#'   value is an integer for [isSingleIntegerInRange()].
#'
#' @param max a single numeric value. Upper inclusive boundary for `x`. This
#'   value is an integer for [isSingleIntegerInRange()].
#'
#' @details
#' [isSingleChar()] returns `TRUE` for any non-NA character string (empty
#' or non-empty).
#'
#' [isSingleIntegerInRange()] returns `TRUE` for any single, non-NA integer
#' value falling in the range `[min, max]`.
#'
#' @returns
#' A single logical. `TRUE` is returned unless `x` fails the test.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname utils-is
#'
#' @keywords internal
isString <- function(x) {
    return(is.character(x) && length(x) == 1L && !is.na(x))
}

isNonEmptyString <- function(x) {
    return(isString(x) && nzchar(x))
}

#' @rdname utils-is
isSingleIntegerInRange <- function(x, min = -max, max = .Machine$integer.max) {
    return(is.integer(x) && length(x) == 1L && !is.na(x) && x >= min && x <= max)
}
