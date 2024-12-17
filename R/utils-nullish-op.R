#' Nullish Coalescing Operator
#'
#' A logical operator that returns its right-hand side operand when its
#' left-hand side operand is `NULL`. Otherwise, its left-hand side operand
#' is returned.
#'
#' `%??%` is identical to `%||\%`, another nullish coalescing operator
#' introduced in R version 4.4.0 in package \pkg{base}. It is redefined
#' here for convenience (and for earlier versions of \R) until further
#' notice.
#'
#' @param x,y Any \R objects.
#'
#' @returns
#' Argument `y` if `x` is `NULL`, and argument `x` otherwise.
#'
#' @noRd
`%??%` <- function(x, y) {
    return(if (is.null(x)) y else x)
}
