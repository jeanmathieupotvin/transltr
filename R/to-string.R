to_string <- function(x, ...) {
    UseMethod("to_string")
}

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
