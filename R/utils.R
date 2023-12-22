#' Other miscellaneous tools
#'
#' Small miscelleneous functions.
#'
#' @param lhs,rhs any \R object.
#'
#' @details
#' `%||%` is the usual null coalescing operator.
#'
#' @returns
#' `%||%` returns returns `rhs` whenever `lhs` evaluates to `NULL`.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname utils
#'
#' @keywords internal
trickRoxyygenTemporarily <- function() {
    # FIXME: replace me by next misc function.
    return(.NotYetImplemented())
}

#' @rdname utils
`%??%` <- function(lhs, rhs) {
    return(if (is.null(lhs)) rhs else lhs)
}
