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


#' Format Vectors
#'
#' Format atomic [vectors][vector()] and/or *recursive* structures such as
#' [lists][list()].
#'
#' The intent of [format_vector()] is to format any [vector][vector()] into a
#' set of name/value pairs (given as character strings), where indentation
#' provides information on embedded structures. The following template shows
#' how it attempts to format `x`.
#'
#' ```
#' <.top_label>:
#'   <names(x[1])>: <x[1]>
#'   <names(x[2])>: <x[2]>
#'   <names(x[3])>:
#'     <names(x[[3]][1])>: <x[[3]][[1]]>
#'     ...
#'   <nokey>: <x[i]>
#'   <nokey>: <x[j]>
#'   <nokey>:
#'     <nokey>: <x[[k]][[1]]>
#'     ...
#' ```
#'
#' Names are used to construct labels. If `x[i]` has a name (`names(x[i])`
#' is not `NULL`), it is used to construct a label for `x[i]`. The format is
#' equivalent to `names(x[i]): as.character(x[i])`. If `x[i]` does not have
#' a name (or if it is empty), `<nokey>` is printed, unless `.show_nokey` is
#' `FALSE`. See Examples below.
#'
#' @param x A [vector][vector()]. It cannot be empty.
#'
#' @param .top_label A `NULL` or a non-empty and non-[NA][base::NA] character
#'   string used as some kind of descriptive message. Its exact meaning depends
#'   on the underlying context. In recursive calls to [format_vector()], this
#'   is set equal to the name of the current element of `x`. **It is not
#'   validated for efficiency.**
#'
#' @param .indent A non-[NA][base::NA] integer value. Number of space characters
#'   to use by `.levels` for indentation purposes. **It is not validated for
#'   efficiency.**
#'
#' @param .show_nokey A non-[NA][base::NA] logical value. Should `NULL` and/or
#'   empty names be replaced by `"<nokey>"`? **It is not validated for
#'   efficiency.**
#'
#' @param .level A non-[NA][base::NA] integer value. The current nesting level
#'   in recursive calls to [format_vector()]. **It is not validated for
#'   efficiency.**
#'
#' @returns A character vector.
#'
#' @note
#' [format_vector()] is the *workhorse* function of all S3 [format()] methods:
#' [format.Translator()], [format.Block()], and [format.Location()].
#'
#' @examples
#' x <- list(
#'     FirstName = "John",
#'     LastName  = "Doe",
#'     Address   = list(
#'         StreetAddress = "123 Main Street",
#'         City          = "Montreal",
#'         Province      = "Quebec",
#'         PostalCode    = "H0H 0H0"),
#'     Notes = c(
#'         "Send mail to",
#'         "address above."))
#'
#' cat(transltr:::format_vector(x, "<JohnDoe>"), sep = "\n")
#' cat(transltr:::format_vector(x, "<JohnDoe>", .show_nokey = FALSE), sep = "\n")
#'
#' @rdname format-vector
#' @family utility functions
#' @keywords internal
format_vector <- function(
    x          = vector(),
    .top_label  = NULL,
    .indent     = 2L,
    .show_nokey = TRUE,
    .level      = 1L)
{
    prefix  <- strrep(" ", .indent * .level)
    lines   <- vector("list", length(x) + 1L)
    indices <- seq_along(x)

    lines[[1L]] <- .top_label

    for (i in indices) {
        xi       <- x[i]
        xi_names <- names(xi) %??% ""

        if (.show_nokey && !nzchar(xi_names)) {
            xi_names <- "<nokey>"
        }

        labels <- if (is.null(xi_names) || !nzchar(xi_names)) {
            prefix
        } else {
            sprintf("%s%s: ", prefix, xi_names)
        }

        # Vectors and recursive structures.
        if (length(xi[[1L]]) > 1L) {
            lines[[i + 1L]] <- format_vector(xi[[1L]],
                labels,
                .indent,
                .show_nokey,
                .level + 1L)
            next
        }

        # Single values (of length 1).
        lines[[i + 1L]] <- sprintf("%s%s", labels, xi)
    }

    return(str_trim(unlist(lines, TRUE, FALSE)))
}


# `%||%` was introduced in R 4.4.0. We redefine it here for
# convenience (and for earlier versions of R) until further
# notice as an undocumented and internal operator. To avoid
# naming collisions, we rename it `%??%`.
`%??%` <- function(x, y) if (is.null(x)) y else x
