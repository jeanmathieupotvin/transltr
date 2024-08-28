is_chr <- function(x, accept_empty = FALSE) {
    return(is.character(x) && (length(x) || accept_empty) && !anyNA(x))
}

is_int1 <- function(x) {
    return(is.integer(x) && length(x) == 1L && !is.na(x))
}

is_chr1 <- function(x, accept_empty_string = FALSE) {
    return(
        is.character(x) &&
        length(x) == 1L &&
        (nzchar(x) || accept_empty_string) &&
        !is.na(x))
}

is_list <- function(x, accept_empty = FALSE) {
    return(is.list(x) && (length(x) || accept_empty))
}

is_named <- function(x, accept_empty_names = FALSE, accept_na_names = FALSE) {
    if (length(x)) {
        x_names <- names(x)

        return(
            !is.null(x_names) &&
            (all(nzchar(x_names)) || accept_empty_names) &&
            (!anyNA(x_names) || accept_na_names))
    }

    # Our convention is to consider empty
    # vectors as being properly named.
    return(TRUE)
}

assert_chr <- function(
    x            = character(),
    accept_empty = FALSE,
    throw_error  = TRUE)
{
    err_msg <- ""

    if (!is_chr(x, accept_empty)) {
        err_msg <- sprintf(
            "'%s' must be a%s character vector of non-NA values.",
            deparse(substitute(x)),
            if (accept_empty) "" else " non-empty")

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

assert_chr1 <- function(
    x                   = "",
    accept_empty_string = FALSE,
    throw_error         = TRUE)
{
    err_msg <- ""

    if (!is_chr1(x, accept_empty_string)) {
        err_msg <- sprintf(
            "'%s' must be a non-NA%s character of length 1.",
            deparse(substitute(x)),
            if (accept_empty_string) "" else " and non-empty")

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

assert_int1 <- function(x = 0L, throw_error = TRUE) {
    err_msg <- ""

    if (!is_int1(x)) {
        err_msg <- sprintf(
            "'%s' must be a non-NA integer of length 1.",
            deparse(substitute(x)))

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

assert_list <- function(x = list(), accept_empty = FALSE, throw_error = TRUE) {
    err_msg <- ""

    if (!is_list(x, accept_empty)) {
        err_msg <- sprintf(
            "'%s' must be a%s list.",
            deparse(substitute(x)),
            if (accept_empty) "" else " non-empty")

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

assert_names <- function(
    x,
    accept_empty_names = FALSE,
    accept_na_names    = FALSE,
    throw_error        = TRUE)
{
    err_msg <- ""

    if (!is_named(x, accept_empty_names, accept_na_names)) {
        err_msg <- sprintf(
            "'%s' must have names.%s%s",
            deparse(substitute(x)),
            if (accept_empty_names) " They can be empty strings." else "",
            if (accept_na_names)    " They can be NA values."     else "")

        if (throw_error) stops(err_msg)
    }

    return(err_msg)
}

# This is a refactoring of base::match.arg().
assert_choice <- function(x, quote_values = FALSE, throw_error = TRUE) {
    i_stack   <- sys.parent()
    parent    <- sys.frame(i_stack)
    a_formals <- formals(sys.function(i_stack))
    a_name    <- deparse(substitute(x))
    a_values  <- eval(a_formals[[a_name]], parent)

    # If arg is identical to a_values, this implies
    # user did not specify a specific value and is
    # requesting the default one.
    a_value <- if (identical(x, a_values)) {
        a_values[[1L]]
    } else if (is.na(i_match <- pmatch(x, a_values))) {
        err_msg <- sprintf(
            "'%s' must be equal to %s.",
            a_name,
            to_string(a_values, quote_values))

        if (throw_error) stops(err_msg)
        return(err_msg)
    } else {
        a_values[[i_match]]
    }

    assign(a_name, a_value, parent)
    return("")
}
