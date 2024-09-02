#' Test objects
#'
#' @description
#' Functions below internally streamline usage of defensive programming
#' through *functional guard clauses* (guard clauses as functions).
#'
#' `is_*()` functions check whether their argument meets condition(s) and
#' always returns a logical. Each `is_*()` function has a corresponding
#' `assert_*()` function that further throws an error message when these
#' condition(s) are not met.
#'
#' @details
#' Guard clauses are quite useful when writing more robust code. However, they
#' tend to be verbose and recycled within a project. Manually copying them
#' multiple times makes it harder to keep error messages consistent over
#' time. The `assert_*()` family solves these challenges by encapsulating
#' guard clause into simple semantic functions. See Examples below.
#'
#' By convention, [NA][base::NA] values are **always** disallowed. This is
#' because [transltr] rarely uses them.
#'
#' @param x An object to be tested.
#'
#' @param x_name The underlying name of `x`.
#'
#' @param min A numeric lower bound. It can be infinite.
#'
#' @param max A numeric upper bound. It can be infinite.
#'
#' @param choices A [vector][base::vector] of valid candidates for `x`.
#'
#' @param quote_values Should `choices` be quoted by single quotation marks?
#'
#' @param allow_empty Should empty vectors of length 0 be considered as
#'   valid values?
#'
#' @param allow_empty_string Should empty character strings be considered as
#'   valid values?
#'
#' @param allow_empty_names Should empty character strings be considered as
#'   valid names? Note that this is different from having no names at all.
#'
#' @param allow_na_names Should NA values be considered as valid names?
#'
#' @param allow_partial Should `x` be partially matched? If so,
#'   [base::pmatch()] is used. Note that [base::match()] is used by default.
#'   The former is used to mimic the behavior of [base::match.arg()].
#'
#' @param throw_error Should an error be thrown? If so, they are returned
#'   by [stops()].
#'
#' @returns
#' [is_chr()],
#' [is_int1()],
#' [is_chr1()],
#' [is_list()],
#' [is_between()],
#' [is_named()], and
#' [is_match()] all return a logical value.
#'
#' [assert_chr()],
#' [assert_int1()],
#' [assert_chr1()],
#' [assert_list()],
#' [assert_between()],
#' [assert_named()],
#' [assert_match()],
#' [assert_arg()] all return an empty character string if `x` meets the
#' underlying condition(s) specified by the function. Otherwise, they
#' throw an error unless `throw_error` is `FALSE`. In that case, the
#' error message is returned as a character string.
#'
#' @note
#' [assert_arg()] is a refactoring of [base::match.arg()] and relies on
#' [assert_match()] internally and does not have a direct `is_arg()`
#' equivalent. It is designed to be called within another function.
#'
#' @seealso [stops()]
#'
#' @examples
#' x <- "my string"
#'
#' ## Here is a guard clause that checks whether an input is a character
#' ## string. While this works fine, having to deal with many similar
#' ## structures across the whole project is cumbersome.
#' if (is.character(x) && length(x) == 1L && nzchar(x) && !is.na(x))) {
#'     stop("'x' must be a must be a non-NA and non-empty character of length 1.")
#' }
#'
#' ## The call to if () can be simplified with is_chr1().
#' if (is_chr1(x, allow_empty_string = FALSE)) {
#'     stop("'x' must be a must be a non-NA and non-empty character of length 1.")
#' }
#'
#' ## The whole structure can be replaced by a single call to assert_chr1().
#' assert_chr1(x)
#'
#' @rdname assert
#' @family is
#' @keywords internal
is_chr <- function(x, allow_empty = FALSE) {
    return(is.character(x) && (length(x) || allow_empty) && !anyNA(x))
}

#' @rdname assert
#' @family is
#' @keywords internal
is_int1 <- function(x) {
    return(is.integer(x) && length(x) == 1L && !is.na(x))
}

#' @rdname assert
#' @family is
#' @keywords internal
is_chr1 <- function(x, allow_empty_string = FALSE) {
    return(
        is.character(x) &&
        length(x) == 1L &&
        (nzchar(x) || allow_empty_string) &&
        !is.na(x))
}

#' @rdname assert
#' @family is
#' @keywords internal
is_list <- function(x, allow_empty = FALSE) {
    return(is.list(x) && (length(x) || allow_empty))
}

#' @rdname assert
#' @family is
#' @keywords internal
is_between <- function(x, min = -Inf, max = Inf) {
    return(is.numeric(x) && length(x) == 1L && !is.na(x) && x >= min && x <= max)
}

#' @rdname assert
#' @family is
#' @keywords internal
is_named <- function(x, allow_empty_names = FALSE, allow_na_names = FALSE) {
    if (length(x)) {
        x_names <- names(x)

        return(
            !is.null(x_names) &&
            (all(nzchar(x_names)) || allow_empty_names) &&
            (!anyNA(x_names) || allow_na_names))
    }

    # Empty vectors are considered to be named.
    return(TRUE)
}

#' @rdname assert
#' @family is
#' @keywords internal
is_match <- function(x, choices, allow_partial = FALSE) {
    if (length(x)) {
        local_match <- if (allow_partial) base::pmatch else base::match
        return(local_match(x[[1L]], choices, 0L) > 0L)
    }

    # Empty vectors are considered as
    # being non-comparable (no match).
    return(FALSE)
}

#' @rdname assert
#' @family assert
#' @keywords internal
assert_chr <- function(
    x,
    allow_empty = FALSE,
    throw_error = TRUE,
    x_name      = deparse(substitute(x)))
{
    err_msg <- ""

    if (!is_chr(x, allow_empty)) {
        err_msg <- sprintf(
            "'%s' must be a%s character vector of non-NA values.",
            x_name,
            if (allow_empty) "" else " non-empty")

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

#' @rdname assert
#' @family assert
#' @keywords internal
assert_int1 <- function(
    x,
    throw_error = TRUE,
    x_name      = deparse(substitute(x)))
{
    err_msg <- ""

    if (!is_int1(x)) {
        err_msg <- sprintf(
            "'%s' must be a non-NA integer of length 1.",
            x_name)

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

#' @rdname assert
#' @family assert
#' @keywords internal
assert_chr1 <- function(
    x,
    allow_empty_string = FALSE,
    throw_error        = TRUE,
    x_name             = deparse(substitute(x)))
{
    err_msg <- ""

    if (!is_chr1(x, allow_empty_string)) {
        err_msg <- sprintf(
            "'%s' must be a non-NA%s character of length 1.",
            x_name,
            if (allow_empty_string) "" else " and non-empty")

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

#' @rdname assert
#' @family assert
#' @keywords internal
assert_list <- function(
    x,
    allow_empty = FALSE,
    throw_error = TRUE,
    x_name      = deparse(substitute(x)))
{
    err_msg <- ""

    if (!is_list(x, allow_empty)) {
        err_msg <- sprintf(
            "'%s' must be a%s list.",
            x_name,
            if (allow_empty) "" else " non-empty")

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

#' @rdname assert
#' @family assert
#' @keywords internal
assert_between <- function(
    x,
    min         = -Inf,
    max         = Inf,
    throw_error = TRUE,
    x_name      = deparse(substitute(x)))
{
    err_msg <- ""

    if (!is_between(x, min, max)) {
        is_inf <- is.infinite(c(min, max))

        # The range printed as part of the error
        # message can be missing or equal to
        # [value, value],
        # (-Inf, value], or
        # [value, Inf).
        err_msg <- if (!all(is_inf)) {
            sprintf(
                "'%s' must be a non-NA numeric value in the range %s%s, %s%s.",
                x_name,
                if (is_inf[[1L]]) "(" else "[",
                as.character(min),
                as.character(max),
                if (is_inf[[2L]]) ")" else "]")
        } else {
            sprintf("'%s' must be a non-NA numeric value.", x_name)
        }

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

#' @rdname assert
#' @family assert
#' @keywords internal
assert_names <- function(
    x,
    allow_empty_names = FALSE,
    allow_na_names    = FALSE,
    throw_error       = TRUE,
    x_name            = deparse(substitute(x)))
{
    err_msg <- ""

    if (!is_named(x, allow_empty_names, allow_na_names)) {
        err_msg <- sprintf(
            "'%s' must have names.%s%s",
            x_name,
            if (allow_empty_names) " They can be empty strings." else "",
            if (allow_na_names)    " They can be NA values."     else "")

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

#' @rdname assert
#' @family assert
#' @keywords internal
assert_match <- function(
    x,
    choices,
    allow_partial = FALSE,
    quote_values  = FALSE,
    throw_error   = TRUE,
    x_name        = deparse(substitute(x)))
{
    err_msg <- ""

    if (!is_match(x, choices, allow_partial)) {
        err_msg <- sprintf(
            "'%s' must be equal to %s.",
            x_name,
            to_string(choices, quote_values))

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

#' @rdname assert
#' @family assert
#' @keywords internal
assert_arg <- function(
    x,
    quote_values = FALSE,
    throw_error  = TRUE)
{
    # What follows is a (partial) refactoring of base::match.arg().
    # Many thanks to the original authors.
    err_msg   <- ""
    i_stack   <- sys.parent()
    parent    <- sys.frame(i_stack)
    x_formals <- formals(sys.function(i_stack))
    x_name    <- deparse(substitute(x))
    x_choices <- eval(x_formals[[x_name]], parent)

    # If arg's value (which is x) is identical to x_choices,
    # this means that the user did not choose a value and is
    # requesting the default one. Just like base::match.arg()
    # the first value extracted from the formal argument is
    # returned.
    if (identical(x, x_choices)) {
        assign(x_name, x_choices[[1L]], parent)
        return("")
    }

    return(
        assert_match(
            x,
            choices       = x_choices,
            allow_partial = TRUE,
            quote_values  = quote_values,
            throw_error   = throw_error,
            x_name        = x_name))
}
