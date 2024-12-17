#' Throw Errors
#'
#' @description
#' [stops()] is equivalent to `stop(..., call. = FALSE)`. It removes calls
#' from error messages by default. These are rarely useful and confuse users
#' more often than they help them.
#'
#' [stopf()] is equivalent to `stops(sprintf(fmt, ...))`. It wraps
#' [base::sprintf()] and [stops()] and is used to construct flexible
#' error messages.
#'
#' @param fmt A character of length 1 passed as is to [base::sprintf()].
#'
#' @param ... Further arguments respectively passed to [base::stop()] and
#'   [base::sprintf()] by [stops()] and [stopf()].
#'
#' @returns Nothing. These functions are used for their side-effect of raising
#'   an error.
#'
#' @rdname utils-stop
#' @family utility functions
#' @keywords internal
stops <- function(...) {
    stop(..., call. = FALSE)
}

#' @rdname utils-stop
#' @keywords internal
stopf <- function(fmt = "", ...) {
    stops(sprintf(fmt, ...))
}
