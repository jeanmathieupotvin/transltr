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
            padChr(.names),
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

`%?%` <- function(lhs, rhs) {
    return(if (nzchar(lhs)) lhs else rhs)
}

`%??%` <- function(lhs, rhs) {
    return(if (is.null(lhs)) rhs else lhs)
}
