#' Manipulate Strings
#'
#' @description
#' These functions perform common string transformations.
#'
#' @details
#' [str_strip_empty()] strips leading and/or trailing empty elements from
#' a character vector.
#'
#' [str_left_pad()] appends a single character to elements of a character
#' vector until they reach a target length.
#'
#' [str_trim()] wraps [base::strtrim()] and trims elements of a character
#' vector until they reach a target length.
#'
#' @param x A character vector. It can be empty or contain empty elements.
#'
#' @param which A character string equal to `both`, `leading`, or `trailing`.
#'   What to remove from `x`.
#'
#' @param len A non-[NA][base::NA] integer. Desired length for individual
#'   elements of `x` as reported by
#'   [`base::nchar(, type = "chars")`][base::nchar()]. It can further be
#'   `NULL` for [str_left_pad()]. This internally sets `len` equal to
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
#' @examples
#' transltr:::str_strip_empty("") # character(0)
#'
#' x <- c("", "", "a", "b", "", "c", "")
#'
#' transltr:::str_strip_empty(x)             # c("a", "b", "", "c")
#' transltr:::str_strip_empty(x, "leading")  # c("a", "b", "", "c", "")
#' transltr:::str_strip_empty(x, "trailing") # c("", "", "a", "b", "", "c")
#'
#' @rdname strings
#' @family string functions
#' @keywords internal
str_strip_empty <- function(
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
str_left_pad <- function(x = character(), len = NULL, pad = " ") {
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
str_trim <- function(x = character(), len = 80L) {
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
