#' Character String Utilities
#'
#' @description
#' [to_string()] converts an \R object to a character string. It is a slightly
#' more flexible alternative to [base::toString()].
#'
#' [str_trim()] wraps [base::strtrim()] and further adds a `...` suffix to
#' each trimmed element.
#'
#' @details
#' [to_string()] concatenates all elements with `", "`, except for the last
#' one. See argument `last_sep`.
#'
#' @param x Any \R object for [to_string()]. A character vector for [str_trim()].
#'
#' @param quote_values A non-[NA][base::NA] logical value. Should elements of
#'   `x` be quoted?
#'
#' @param last_sep A non-empty and non-[NA][base::NA] character string
#'   separating the last element from the penultimate one.
#'
#' @param ... Further arguments passed to, or from other methods.
#'
#' @param width A non-[NA][base::NA] integer value. The target width for
#'   individual elements of `x`. It takes 3 more characters into account
#'   for the `...` suffix.
#'
#' @returns
#' [to_string()] returns a character string.
#'
#' [str_trim()] returns a character vector having the same length as `x`.
#'
#' @examples
#' to_string(c(1L, 2L, 3L))            ## Outputs "1, 2, or 3"
#' to_string(letters, TRUE)            ## Outputs "'a', 'b', 'c', ..., or 'z'"
#' to_string(letters, TRUE, ", and ")  ## Outputs "'a', 'b', 'c', ..., and 'z'"
#'
#' x <- strrep("a", 200L)
#'
#' # x is reduced to a width of 80 characters (by default). This includes 3
#' # characters for the '...' suffix added and therefore, x is reduced to 77
#' # chars. The same logic is applied  for any width strictly greater than 2.
#' str_trim(x)
#'
#' @rdname utils-strings
#' @family utility functions
#' @keywords internal
to_string <- function(x, ...) {
    UseMethod("to_string")
}

#' @rdname utils-strings
#' @keywords internal
#' @export
to_string.default <- function(x, quote_values = FALSE, last_sep = ", or ", ...) {
    assert_lgl1(quote_values)
    assert_chr1(last_sep)

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

#' @rdname utils-strings
#' @keywords internal
str_trim <- function(x = character(), width = 80L) {
    assert_chr(x, TRUE)
    assert_int1(width)
    assert_between(width, 3L)

    if (!length(x)) {
        return(character())
    }

    to_trim    <- nchar(x) > width
    x[to_trim] <- paste0(strtrim(x[to_trim], width - 3L), "...")
    return(x)
}
