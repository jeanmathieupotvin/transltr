#' Safe Apply Wrappers
#'
#' These functions wrap a function of the [`*apply()`][base::lapply()]
#' family and enforce various values for convenience and additional
#' safety.
#'
#' @param x Passed as is to argument `X` of [`*apply()`][base::lapply()].
#'
#' @param fun Passed as is to argument `FUN` of [`*apply()`][base::lapply()].
#'
#' @param ... Further arguments passed as is to [`*apply()`][base::lapply()].
#'   It later passes them to `fun`.
#'
#' @returns
#' [vapply_1l()],
#' [vapply_1l()], and
#' [vapply_1c()] respectively return a logical, an integer, and a character
#' vector having the same length as `x`. Names are always discarded.
#'
#' @rdname apply-wrappers
#' @family utility functions
#' @keywords internal
vapply_1l <- function(x, fun, ...) {
    return(vapply(x, fun, NA, ..., USE.NAMES = FALSE))
}

#' @rdname apply-wrappers
#' @keywords internal
vapply_1i <- function(x, fun, ...) {
    return(vapply(x, fun, NA_integer_, ..., USE.NAMES = FALSE))
}

#' @rdname apply-wrappers
#' @keywords internal
vapply_1c <- function(x, fun, ...) {
    return(vapply(x, fun, NA_character_, ..., USE.NAMES = FALSE))
}


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
#' @rdname stop
#' @family utility functions
#' @keywords internal
stops <- function(...) {
    stop(..., call. = FALSE)
}

#' @rdname stop
#' @keywords internal
stopf <- function(fmt = "", ...) {
    stops(sprintf(fmt, ...))
}


#' Divide Into Groups
#'
#' [split_ul()] wraps [base::split()] and returns an **u**nnamed **l**ist.
#'
#' @param ... Potential arguments passed to [base::split()].
#'
#' @returns
#' A list. See [base::split()] for further information.
#'
#' @rdname split-ul
#' @family utility functions
#' @keywords internal
split_ul <- function(...) {
    x <- split(...)
    names(x) <- NULL
    return(x)
}


#' Strip Leading and Trailing Empty Strings
#'
#' Strip leading and/or trailing superfluous empty elements from a character
#' vector, keeping only non-empty elements and empty strings intertwined with
#' them.
#'
#' @param x A character vector. It can be empty or contain only empty elements.
#'
#' @param which A character string equal to `both`, `leading`, or `trailing`.
#'   What to remove from `x`.
#'
#' @returns A character vector.
#'
#' @note
#' To strip all empty strings, see [base::nzchar()].
#'
#' @examples
#' transltr:::strip_empty_strings("") # character(0)
#'
#' x <- c("", "", "a", "b", "", "c", "")
#'
#' transltr:::strip_empty_strings(x)             # c("a", "b", "", "c")
#' transltr:::strip_empty_strings(x, "leading")  # c("a", "b", "", "c", "")
#' transltr:::strip_empty_strings(x, "trailing") # c("", "", "a", "b", "", "c", "")
#'
#' @rdname strip-empty-strings
#' @family utility functions
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
        return(character(0L))
    }

    nz_pos <- which(match(is_nz, TRUE, 0L) == 1L)
    start  <- if (which == "trailing") 1L        else min(nz_pos)
    end    <- if (which == "leading")  length(x) else max(nz_pos)
    return(x[seq.int(start, end, 1L)])
}


# `%||%` was introduced in R 4.4.0. We redefine it here for
# convenience (and for earlier versions of R) until further
# notice as an undocumented and internal operator. To avoid
# naming collisions, we rename it `%??%`.
`%??%` <- function(x, y) if (is.null(x)) y else x
