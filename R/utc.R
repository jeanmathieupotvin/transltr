#' Get current date and time
#'
#' @description
#' Get the current Coordinated Universal Time (UTC).
#'
#' @details
#' The format is
#'
#' ```r
#' "<Full Month>, <Day of Month>, <Year> @ <Hours>:<Minutes>:<Seconds> (UTC)".
#' ```
#'
#' @returns A character string.
#'
#' @note
#' The [utc()] function is defined in its own script because it has its
#' own test script. Keeping it separate from other utility functions is
#' better for readability.
#'
#' @rdname utc
#' @family utility functions
#' @keywords internal
utc <- function() {
    return(format(Sys.time(), tz = "UTC", format = "%B %d, %Y @ %T (%Z)"))
}
