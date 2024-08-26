assert_chr <- function(x, accept_empty = FALSE) {
    if (!is_chr(x, accept_empty)) {
        stopf(
            "'%s' must be a character vector of non-NA values.",
            deparse(substitute(x)))
    }

    return(invisible(x))
}

assert_int1 <- function(x) {
    if (!is_int1(x)) {
        stopf(
            "'%s' must be a non-NA integer of length 1.",
            deparse(substitute(x)))
    }

    return(invisible(x))
}

assert_chr1 <- function(x, accept_empty_string = FALSE) {
    if (!is_chr1(x, accept_empty_string)) {
        stopf(
            "'%s' must be a non-NA character of length 1.%s",
            deparse(substitute(x)),
            if (accept_empty_string) " It can be empty." else "")
    }

    return(invisible(x))
}

assert_names <- function(x, accept_empty_names = FALSE, accept_na_names = FALSE) {
    if (!is_named(x, accept_empty_names, accept_na_names)) {
        stopf(
            "'%s' must have valid names. They cannot be NULL.%s%s",
            deparse(substitute(x)),
            if (accept_empty_names) " They can be empty."       else "",
            if (accept_na_names)    " They can be NA values." else "")
    }

    return(invisible(x))
}

# This is a refactoring of base::match.arg().
assert_choice <- function(x, quote_values = FALSE) {
    i_stack   <- sys.parent()
    parent    <- sys.frame(i_stack)
    a_formals <- formals(sys.function(i_stack))
    a_name    <- as.character(substitute(x))
    a_values  <- eval(a_formals[[a_name]], parent)

    # If arg is identical to a_values, this implies
    # user did not specify a specific value and is
    # requesting the default one.
    a_value <- if (identical(x, a_values)) {
        a_values[[1L]]
    } else {
        if (is.na(i_match <- pmatch(x, a_values))) {
            stopf(
                "'%s' must be equal to %s.",
                a_name,
                to_string(a_values, quote_values))
        }

        a_values[[i_match]]
    }

    return(assign(a_name, a_value, parent))
}


# is_*() functions -------------------------------------------------------------


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
