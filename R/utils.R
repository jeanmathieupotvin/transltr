formatNamedValues <- function(..., sep = "  ", indent = 1L) {
    assertString(sep)
    assertSingleIntInRange(indent, 0L)

    if (!is.character(values <- c(...))) {
        values <- as.character(values)
    }
    if (anyNA(names <- names(values) %??% character(length(values)))) {
        names[is.na(names)] <- ""
    }

    return(paste0(strrep(" ", indent), strpad(names), sep, values))
}

trimParsedExpr <- function(expr, width = integer(1L)) {
    assertSingleIntInRange(width, 1L)

    if (nchar(expr <- deparse1(expr)) > width) {
        expr <- sprintf("%s...", strtrim(expr, width))
    }

    return(expr)
}

formatDateTime <- function(x, utc = FALSE) {
    assertSingleLgl(utc)
    return(
        format(x,
            tz     = if (utc) "UTC" else "",
            format = "%A, %Y-%m-%d, %T (%Z)"))
}

`%?%` <- function(lhs, rhs) {
    return(if (length(lhs) && any(nzchar(lhs))) lhs else rhs)
}

`%??%` <- function(lhs, rhs) {
    return(if (is.null(lhs)) rhs else lhs)
}
