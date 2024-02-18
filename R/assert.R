assertString <- function(x) {
    if (!isString(x)) {
        halt(
            "'%s' must be a non-NA character of length 1. It can be empty.",
            deparse(substitute(x)),
            caller = getCallerName(-3L))
    }

    return(invisible(x))
}

assertNonEmptyString <- function(x) {
    if (!isNonEmptyString(x)) {
        halt(
            "'%s' must be a non-NA and non-empty character of length 1.",
            deparse(substitute(x)),
            caller = getCallerName(-3L))
    }

    return(invisible(x))
}

assertSingleLgl <- function(x) {
    if (!isSingleLgl(x)) {
        halt(
            "'%s' must be equal to 'TRUE' or 'FALSE'.",
            deparse(substitute(x)),
            caller = getCallerName(-3L))
    }

    return(invisible(x))
}

assertSingleIntInRange <- function(x, min = -max, max = .Machine$integer.max) {
    if (!isSingleIntInRange(x, min, max)) {
        cl <- match.call()

        halt(
            "'%s' must be a non-NA integer value of length 1%s.",
            deparse(substitute(x)),
            RangeString(cl$min, cl$max),
            caller = getCallerName(-3L))
    }

    return(invisible(x))
}

assertSingleDblInRange <- function(
    x,
    min = .Machine$double.xmin,
    max = .Machine$double.xmax)
{
    if (!isSingleDblInRange(x, min, max)) {
        cl <- match.call()

        halt(
            "'%s' must be a non-NA double value of length 1%s.",
            deparse(substitute(x)),
            RangeString(cl$min, cl$max),
            caller = getCallerName(-3L))
    }

    return(invisible(x))
}

# This is a refactoring of base::match.arg() with a transtlr
# touch that enforces single values by defaut. It must be
# called within a parent function. This is what it means to
# 'match an argument'.
assertChoice <- function(x) {
    stackIndex <- sys.parent()
    formalArgs <- formals(sys.function(stackIndex))
    argName    <- as.character(substitute(x))
    choices    <- eval(formalArgs[[argName]], sys.frame(stackIndex))

    # If arg is identical to choices, this implies
    # user did not specify a specific value and is
    # requesting the default one.
    if (identical(x, choices)) {
        return(invisible(choices[[1L]]))
    }

    if (is.na(matchIndex <- pmatch(x, choices, NA_integer_))) {
        # This creates a numbered list of choices
        # to be printed as part of the error below.
        listedChoices <- paste0(
            sprintf(" [%i] '%s'\n", seq_along(choices), choices),
            collapse = "")

        halt("'%s' must be equal to one of the listed value below.\n%s",
            argName,
            listedChoices,
            caller = getCallerName(-3L))
    }

    return(invisible(choices[[matchIndex]]))
}

RangeString <- function(min = NULL, max = NULL) {
    strings <- c(
        if (!is.null(min)) sprintf(" greater than or equal to %s", min),
        if (!is.null(max)) sprintf(" lower than or equal to %s",   max))
    return(paste0(strings, collapse = " and"))
}
