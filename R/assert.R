is_chr <- function(x, allow_empty = FALSE) {
    return(is.character(x) && (length(x) || allow_empty) && !anyNA(x))
}

is_int1 <- function(x) {
    return(is.integer(x) && length(x) == 1L && !is.na(x))
}

is_chr1 <- function(x, allow_empty_string = FALSE) {
    return(
        is.character(x) &&
        length(x) == 1L &&
        (nzchar(x) || allow_empty_string) &&
        !is.na(x))
}

is_list <- function(x, allow_empty = FALSE) {
    return(is.list(x) && (length(x) || allow_empty))
}

is_between <- function(x, min = -Inf, max = Inf) {
    return(is.numeric(x) && length(x) == 1L && !is.na(x) && x >= min && x <= max)
}

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

is_match <- function(x, choices, allow_partial = FALSE) {
    if (length(x)) {
        local_match <- if (allow_partial) base::pmatch else base::match
        return(local_match(x[[1L]], choices, 0L) > 0L)
    }

    # Empty vectors are considered as
    # being non-comparable (no match).
    return(FALSE)
}

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
