to_string <- function(x, ...) {
    UseMethod("to_string")
}

#' @export
to_string.default <- function(x, quote_values = FALSE, last_sep = ", or ", ...) {
    x <- as.character(x, ...)

    if (quote_values) {
        x <- sprintf("'%s'", x)
    }
    if (length(x) < 2L) {
        return(x)
    }

    return(
        paste0(
            paste0(head(x, -1L), collapse = ", "),
            last_sep,
            tail(x, 1L)))
}
