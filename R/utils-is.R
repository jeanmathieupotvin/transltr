#' Internal tools: test for specific shapes and values
#'
#' Check if a value has a specific shape ([type][base::typeof()],
#' [length][length()]) and/or value ([NAs][base::NA], boundaries, etc.).
#'
#' @param x any \R object to be tested.
#'
#' @details
#' [isSingleChar()] returns `TRUE` for any empty or non-empty character string.
#'
#' @returns
#' A single logical. `TRUE` is returned unless `x` fails the underlying
#' test.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @family internal tools
#'
#' @rdname utils-is
#'
#' @keywords internal
isSingleChar <- function(x) {
    return(is.character(x) && length(x) == 1L && !is.na(x))
}
