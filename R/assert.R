#' Assertions
#'
#' @description
#' These functions are a functional implementation of defensive programming.
#'
#' `is_*()` functions check whether their argument meets certain criteria.
#'
#' `assert_*()` functions further throw an error message when at least one
#' criterion is not met.
#'
#' **Arguments listed below are not explicitly validated for efficiency.**
#'
#' @details
#' Guard clauses tend to be verbose and recycled many times within a project.
#' This makes it hard to keep error messages consistent over time. `assert_*()`
#' functions encapsulate usual guard clause into simple semantic functions.
#' This reduces code repetition and number of required unit tests. See
#' Examples below.
#'
#' By convention, [NA][base::NA] values are **always** disallowed. Package
#' [`transltr`][transltr] never uses them for consistency.
#'
#' [assert_arg()] is a partial refactoring of [base::match.arg()]. It relies
#' on [assert_match()] internally and does not have an equivalent `is_arg()`
#' function. It must be called within another function.
#'
#' [assert()] is a S3 generic function that covers specific data structures.
#' Classes (and underlying objects) that do not have an [assert()] method are
#' considered to be valid by default.
#'
#' @param x Any \R object.
#'
#' @param x_name A non-empty and non-[NA][base::NA] character string. The
#'   underlying name of `x`.
#'
#' @param min A non-[NA][base::NA] numeric lower bound. It can be infinite.
#'
#' @param max A non-[NA][base::NA] numeric upper bound. It can be infinite.
#'
#' @param choices A non-empty [vector][base::vector] of valid candidates.
#'
#' @param allow_empty A non-[NA][base::NA] logical value. Should vectors of
#'   length 0 be considered as valid values?
#'
#' @param allow_empty_string A non-[NA][base::NA] logical value. Should empty
#'   character strings be considered as valid values?
#'
#' @param allow_empty_names A non-[NA][base::NA] logical value. Should empty
#'   character strings be considered as valid names? This is different from
#'   having no names at all.
#'
#' @param allow_na_names A non-[NA][base::NA] logical value. Should
#'   [NA][base::NA] values be considered as valid names?
#'
#' @param allow_partial A non-[NA][base::NA] logical value. Should `x` be
#'   partially matched? If so, [base::pmatch()] is used.
#'
#' @param quote_values A non-[NA][base::NA] logical value. Should `choices`
#'   be quoted? This argument is passed to [str_to()].
#'
#' @template param-throw-error
#'
#' @returns
#' [is_int()],
#' [is_chr()],
#' [is_lgl1()],
#' [is_int1()],
#' [is_chr1()],
#' [is_list()],
#' [is_between()],
#' [is_named()], and
#' [is_match()] return a logical value.
#'
#' [assert()],
#' [assert_int()],
#' [assert_chr()],
#' [assert_lgl1()],
#' [assert_int1()],
#' [assert_chr1()],
#' [assert_list()],
#' [assert_between()],
#' [assert_named()],
#' [assert_match()], and
#' [assert_arg()] return an empty character vector if `x` meets the underlying
#' criteria and throw an error otherwise. If `throw_error` is `FALSE`, the
#' error message is returned as a character vector. Unless otherwise stated,
#' the latter is of length 1 (a character string).
#'
#' [assert.default()] always returns an empty character vector.
#'
#' @examples
#' transltr:::is_int(c(1L, 2L))        ## TRUE
#' transltr:::is_int(c(1.0, 2.0))      ## FALSE
#' transltr:::is_chr(c("a", "b"))      ## TRUE
#' transltr:::is_chr(1i)               ## FALSE
#' transltr:::is_lgl1(FALSE)           ## TRUE
#' transltr:::is_lgl1(1L)              ## FALSE
#' transltr:::is_int1(1L)              ## TRUE
#' transltr:::is_int1(1.0)             ## FALSE
#' transltr:::is_chr1("a")             ## TRUE
#' transltr:::is_chr1(1L)              ## FALSE
#' transltr:::is_list(list(1L))        ## TRUE
#' transltr:::is_list(1L)              ## FALSE
#' transltr:::is_between(1, 0, 2)      ## TRUE
#' transltr:::is_between(3, 0, 2)      ## FALSE
#' transltr:::is_named(c(a = 1L))      ## TRUE
#' transltr:::is_named(1L)             ## FALSE
#' transltr:::is_match(1L, c(1L, 2L))  ## TRUE
#' transltr:::is_match(3L, c(1L, 2L))  ## FALSE
#'
#' x <- "my string"
#'
#' # Here is a guard clause that checks whether
#' # x is a non-empty and non-NA character string.
#' if (!is.character(x) || length(x) != 1L || !nzchar(x) || is.na(x)) {
#'   stop("'x' must be a must be a non-NA and non-empty character of length 1.")
#' }
#'
#' # Code above can be replaced by a much more terse call to assert_chr1().
#' transltr:::assert_chr1(x)
#'
#' # assert_*() functions outputs "" unless the input is invalid.
#' # In that case, an error is thrown.
#' transltr:::assert_int(c(1L, 2L))
#' transltr:::assert_chr(c("a", "b"))
#' transltr:::assert_lgl1(FALSE)
#' transltr:::assert_int1(1L)
#' transltr:::assert_chr1("a")
#' transltr:::assert_list(list(1L))
#' transltr:::assert_between(1, 0, 2)
#' transltr:::assert_named(c(a = 1L))
#' transltr:::assert_match(1L, c(1L, 2L))
#'
#' # These calls throw error because inputd are invalid.
#' # They do not meet the underlying criteria.
#' \dontrun{
#' transltr:::assert_int(c(1.0, 2.0))
#' transltr:::assert_chr(1i)
#' transltr:::assert_lgl1(1L)
#' transltr:::assert_int1(1.0)
#' transltr:::assert_chr1(1L)
#' transltr:::assert_list(1L)
#' transltr:::assert_between(3, 0, 2)
#' transltr:::assert_named(1L)
#' transltr:::assert_match(3L, c(1L, 2L))}
#'
#' # assert_arg() is special. It can only be called within
#' # another function, just like base::match.arg().
#' foo <- function(value = c("a", "b", "c")) {
#'   transltr:::assert_arg(value)
#'   return(value)
#' }
#'
#' foo("a")
#' \dontrun{foo("d")  ## Since "d" is not a valid value, an error is thrown.}
#'
#' # assert.default() always returns an empty character vector.
#' assert(structure(list(), class = "MyList"))
#'
#' @rdname assert
#' @keywords internal
is_int <- function(x, allow_empty = FALSE) {
    return(is.integer(x) && (length(x) || allow_empty) && !anyNA(x))
}

#' @rdname assert
#' @keywords internal
is_chr <- function(x, allow_empty = FALSE) {
    return(is.character(x) && (length(x) || allow_empty) && !anyNA(x))
}

#' @rdname assert
#' @keywords internal
is_lgl1 <- function(x) {
    return(is.logical(x) && length(x) == 1L && !is.na(x))
}

#' @rdname assert
#' @keywords internal
is_int1 <- function(x) {
    return(is.integer(x) && length(x) == 1L && !is.na(x))
}

#' @rdname assert
#' @keywords internal
is_chr1 <- function(x, allow_empty_string = FALSE) {
    return(
        is.character(x) &&
        length(x) == 1L &&
        (nzchar(x) || allow_empty_string) &&
        !is.na(x))
}

#' @rdname assert
#' @keywords internal
is_list <- function(x, allow_empty = FALSE) {
    return(is.list(x) && (length(x) || allow_empty))
}

#' @rdname assert
#' @keywords internal
is_between <- function(x, min = -Inf, max = Inf) {
    return(is.numeric(x) && length(x) == 1L && !is.na(x) && x >= min && x <= max)
}

#' @rdname assert
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
#' @keywords internal
is_match <- function(x, choices = vector(), allow_partial = FALSE) {
    if (length(x)) {
        .match <- if (allow_partial) base::pmatch else base::match
        return(.match(x[[1L]], choices, 0L) > 0L)
    }

    # Empty vectors are considered as
    # being non-comparable (no match).
    return(FALSE)
}

#' @rdname assert
#' @keywords internal
assert_int <- function(
    x,
    allow_empty = FALSE,
    throw_error = TRUE,
    x_name      = deparse(substitute(x)))
{
    err_msg <- character()

    if (!is_int(x, allow_empty)) {
        err_msg <- sprintf(
            "'%s' must be a%s integer vector of non-NA values.",
            x_name,
            if (allow_empty) "" else " non-empty")

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

#' @rdname assert
#' @keywords internal
assert_chr <- function(
    x,
    allow_empty = FALSE,
    throw_error = TRUE,
    x_name      = deparse(substitute(x)))
{
    err_msg <- character()

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
#' @keywords internal
assert_lgl1 <- function(
    x,
    throw_error = TRUE,
    x_name      = deparse(substitute(x)))
{
    err_msg <- character()

    if (!is_lgl1(x)) {
        err_msg <- sprintf(
            "'%s' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').",
            x_name)

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

#' @rdname assert
#' @keywords internal
assert_int1 <- function(
    x,
    throw_error = TRUE,
    x_name      = deparse(substitute(x)))
{
    err_msg <- character()

    if (!is_int1(x)) {
        err_msg <- sprintf(
            "'%s' must be a non-NA integer of length 1.",
            x_name)

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

#' @rdname assert
#' @keywords internal
assert_chr1 <- function(
    x,
    allow_empty_string = FALSE,
    throw_error        = TRUE,
    x_name             = deparse(substitute(x)))
{
    err_msg <- character()

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
#' @keywords internal
assert_list <- function(
    x,
    allow_empty = FALSE,
    throw_error = TRUE,
    x_name      = deparse(substitute(x)))
{
    err_msg <- character()

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
#' @keywords internal
assert_between <- function(
    x,
    min         = -Inf,
    max         = Inf,
    throw_error = TRUE,
    x_name      = deparse(substitute(x)))
{
    err_msg <- character()

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
#' @keywords internal
assert_named <- function(
    x,
    allow_empty_names = FALSE,
    allow_na_names    = FALSE,
    throw_error       = TRUE,
    x_name            = deparse(substitute(x)))
{
    err_msg <- character()

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
#' @keywords internal
assert_match <- function(
    x,
    choices,
    allow_partial = FALSE,
    quote_values  = FALSE,
    throw_error   = TRUE,
    x_name        = deparse(substitute(x)))
{
    err_msg <- character()

    if (!is_match(x, choices, allow_partial)) {
        err_msg <- sprintf(
            "'%s' must be equal to %s.",
            x_name,
            str_to(choices, quote_values))

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

#' @rdname assert
#' @keywords internal
assert_arg <- function(
    x,
    quote_values = FALSE,
    throw_error  = TRUE)
{
    # This is a refactoring of base::match.arg().
    # Many thanks to the original authors.
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
        return(character())
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

#' @rdname assert
#' @keywords internal
assert <- function(x, ...) {
    UseMethod("assert")
}

#' @rdname assert
#' @keywords internal
#' @export
assert.default <- function(x, ...) {
    return(character())
}
