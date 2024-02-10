formatNamedValues <- function(..., sep = "  ", indent = 1L) {
    if (!isString(sep)) {
        halt("'sep' must be a non-NA character string. It can be empty.")
    }
    if (!isSingleIntInRange(indent, 0L)) {
        halt("'indent' must be a non-NA integer value of length 1 greater than or equal to 0.")
    }

    if (!is.character(values <- c(...))) {
        values <- as.character(values)
    }

    .names <- names(values) %??% character(length(values))

    if (anyNA(.names)) {
        .names[is.na(.names)] <- ""
    }

    return(
        sprintf("%s%s%s%s",
            strrep(" ", indent),
            strpad(.names),
            sep,
            values))
}

trimParsedExpr <- function(expr, width = integer(1L)) {
    if (!is.language(expr)) {
        halt("'expr' must be a name, a call, or an expression object.")
    }
    if (!isSingleIntInRange(width, 1L)) {
        halt("'width' must be a non-NA integer value of length 1 greater than or equal to 1.")
    }
    if (nchar(expr <- deparse1(expr)) > width) {
        expr <- sprintf("%s...", strtrim(expr, width))
    }

    return(expr)
}

# This is a refactoring of base::match.arg() with a transtlr
# touch on it that enforces single values by defaut. It must
# be called within a parent function. This is what it means
# to 'match an argument'.
.match.arg <- function(arg = "") {
    caller     <- getCallerName(-2L)
    stackIndex <- sys.parent()
    formalArgs <- formals(sys.function(stackIndex))
    argName    <- as.character(substitute(arg))
    choices    <- eval(formalArgs[[argName]], sys.frame(stackIndex))

    # If arg is identical to choices, this implies
    # user did not specify a specific value and is
    # requesting the default one.
    if (identical(arg, choices)) {
        return(choices[[1L]])
    }

    if (!isString(arg)) {
        halt("'%s' must be a non-NA character of length 1. It can be empty.",
            argName,
            caller = caller)
    }

    if ({ matchIndex <- pmatch(arg, choices, nomatch = 0L) } == 0L) {
        # This creates a numbered list of choices
        # to be printed as part of the error below.
        listedChoices <- sprintf(" [%i] '%s'\n", seq_along(choices), choices)

        halt("'%s' must be equal to one of the listed value below.\n%s",
            argName,
            paste0(listedChoices, collapse = ""),
            caller = caller)
    }

    return(choices[[matchIndex]])
}

`%?%` <- function(lhs, rhs) {
    return(if (nzchar(lhs)) lhs else rhs)
}

`%??%` <- function(lhs, rhs) {
    return(if (is.null(lhs)) rhs else lhs)
}
