#' Convert an R object to a character string
#'
#' This is an alternative to [base::toString()] that provides greater
#' flexiblity. It is used internally to construct more meaningful error
#' messages. It drops argument `width` because it is not useful in the
#' context of package \pkg{transltr}.
#'
#' @param x An object to be converted.
#'
#' @param quote_values Should elements of `x` be quoted with single quotation
#'   marks?
#'
#' @param last_sep A character string that separates the last element from
#'   the penultimate one.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @returns A character string constructed from `x`. Elements are separated
#'   by `", "`, except for the last one. Argument `last_sep` is used for it.
#'
#' @rdname to-string
#' @family utility functions
#' @keywords internal
to_string <- function(x, ...) {
    UseMethod("to_string")
}

#' @rdname to-string
#' @keywords internal
#' @export
to_string.default <- function(x, quote_values = FALSE, last_sep = " or ", ...) {
    x <- as.character(x, ...)

    if (quote_values) {
        x <- sprintf("'%s'", x)
    }
    if (length(x) < 2L) {
        return(x)
    }

    return(
        paste0(
            paste0(utils::head(x, -1L), collapse = ", "),
            last_sep,
            utils::tail(x, 1L)))
}
