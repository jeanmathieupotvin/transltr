#' Get Current Date and Time
#'
#' Get the current Coordinated Universal Time (UTC).
#'
#' @details
#' The format is
#'
#' ```r
#' "<Full Month> <Day>, <Year> @ <Hours>:<Minutes>:<Seconds> (UTC)".
#' ```
#'
#' @returns A character string.
#'
#' @rdname utc
#' @family utility functions
#' @keywords internal
utc <- function() {
    return(format(Sys.time(), tz = "UTC", format = "%B %d, %Y @ %T (%Z)"))
}
