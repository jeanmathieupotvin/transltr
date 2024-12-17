#' Divide Into Groups
#'
#' [split_ul()] wraps [base::split()] and returns an **u**nnamed **l**ist.
#'
#' @param ... Further arguments passed to [base::split()].
#'
#' @returns
#' A list. See [base::split()] for further information.
#'
#' @rdname utils-split-ul
#' @family utility functions
#' @keywords internal
split_ul <- function(...) {
    x <- split(...)
    names(x) <- NULL
    return(x)
}
