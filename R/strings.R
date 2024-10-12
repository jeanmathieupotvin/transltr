#' Manipulate Strings
#'
#' @description
#' These functions perform common string transformations.
#'
#' @details
#' [strip_empty_strings()] strips leading and/or trailing empty elements from
#' a character vector.
#'
#' [left_pad_strings()] appends a single character to elements of a character
#' vector until they reach a target length.
#'
#' [trim_strings()] wraps [base::strtrim()] and trims elements of a character
#' vector until they reach a target length.
#'
#' [sanitize_strings()] does three things to elements of a character vector.
#'   * It concatenates its elements into a single one.
#'   * It replaces substrings of repeated space characters (spaces and/or tabs)
#'     by a single space.
#'   * It removes leading space, tab, and new line characters.
#'
#' @param x A character vector. It can be empty or contain empty elements.
#'
#' @param which A character string equal to `both`, `leading`, or `trailing`.
#'   What to remove from `x`.
#'
#' @param len A non-[NA][base::NA] integer. Desired length for individual
#'   elements of `x` as reported by
#'   [`base::nchar(, type = "chars")`][base::nchar()]. It can further be
#'   `NULL` for [left_pad_strings()]. This internally sets `len` equal to
#'   the length of the longest element of `x`.
#'
#' @param pad A non-empty and non-[NA][base::NA] character string used to
#'   separate the results.
#'
#' @param concat A non-[NA][base::NA] character string used to concatenate
#'   elements of `x`.
#'
#' @returns A character vector.
#'
#' @note
#' To strip all empty strings, use [base::nzchar()]. It will be much faster.
#'
#' [sanitize_strings()] will likely change in a neear future. This function
#' is not robust enough. It should support escaped characters, escaped
#' sequences, etc.
#'
#' @examples
#' transltr:::strip_empty_strings("") # character(0)
#'
#' x <- c("", "", "a", "b", "", "c", "")
#'
#' transltr:::strip_empty_strings(x)             # c("a", "b", "", "c")
#' transltr:::strip_empty_strings(x, "leading")  # c("a", "b", "", "c", "")
#' transltr:::strip_empty_strings(x, "trailing") # c("", "", "a", "b", "", "c")
#'
#' @rdname strings
#' @family string functions
#' @keywords internal
strip_empty_strings <- function(
    x     = character(),
    which = c("both", "leading", "trailing"))
{
    assert_chr(x, TRUE)
    assert_arg(which, TRUE)

    is_nz <- nzchar(x)

    # If x only contains empty strings,
    # then they are all removed by design.
    if (all(!is_nz)) {
        return(character())
    }

    nz_pos <- which(match(is_nz, TRUE, 0L) == 1L)
    start  <- if (which == "trailing") 1L        else min(nz_pos)
    end    <- if (which == "leading")  length(x) else max(nz_pos)
    return(x[seq.int(start, end, 1L)])
}

#' @rdname strings
#' @keywords internal
left_pad_strings <- function(x = character(), len = NULL, pad = " ") {
    assert_chr(x, TRUE)
    assert_chr1(pad)

    if (!length(x)) {
        return(character())
    }
    if (nchar(pad) != 1L) {
        stops("'pad' must be a single character.")
    }

    nchars <- nchar(x)
    len    <- len %??% max(nchars)

    assert_int1(len)
    assert_between(len, 0L)
    return(paste0(strrep(pad, pmax(len - nchars, 0L)), x))
}

#' @rdname strings
#' @keywords internal
trim_strings <- function(x = character(), len = 80L) {
    assert_chr(x, TRUE)
    assert_int1(len)
    assert_between(len, 3L)

    if (!length(x)) {
        return(character())
    }

    to_trim    <- nchar(x) > len
    x[to_trim] <- paste0(strtrim(x[to_trim], len - 3L), "...")
    return(x)
}

#' @rdname strings
#' @keywords internal
sanitize_strings <- function(x = character(), concat = " ") {
    # FIXME: this function should be reviewed and robustified.
    assert_chr(x)
    assert_chr1(concat)
    str <- paste0(x, collapse = concat)
    return(gsub("[ \n\t]{2,}", " ", gsub("^[ \n\t]{2,}", "", str)))
}
