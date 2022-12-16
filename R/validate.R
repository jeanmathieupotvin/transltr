validateString <- function(x = character(1L)) {
    if (!is.character(x) || length(x) != 1L || !nzchar(x) || is.na(x)) {
        stopf(
            "TypeError",
            "`%s` must be a non-empty, non-NA character string.",
            deparse(substitute(x)))
    }

    return(invisible(x))
}

validateIndex <- function(x = integer(1L)) {
    if (!is.integer(x) || length(x) != 1L || is.na(x) || x <= 0L) {
        stopf(
            "TypeError",
            "`%s` must be a non-NA, positive integer value.",
            deparse(substitute(x)))
    }

    return(invisible(x))
}

validateFile <- function(file = character(1L)) {
    validateString(file)

    if (!utils::file_test("-f", file)) {
        stopf("InterfaceError", "`file` must be an existing file.", file)
    }

    return(invisible(file))
}

validateTokens <- function(x) {
    xsymbol <- deparse(substitute(x))

    # tokens must be a data.frame created
    # by function utils::getParseData().
    expectedCols <- c(
      # Name     = Type
        line1    = "integer",
        col1     = "integer",
        line2    = "integer",
        col2     = "integer",
        id       = "integer",
        parent   = "integer",
        token    = "character",
        terminal = "logical",
        text     = "character")

    if (!is.data.frame(x)) {
        stopf(
            "TypeError",
            "`%s` must be a `data.frame` object.",
            xsymbol)
    }
    if (!identical(names(tokens), names(expectedCols))) {
        # Quote expected column names and coerce
        # them to a single string where elements
        # are separated by ", ".
        stopf(
            "TypeError",
            "`%s` must have these columns: %s.",
            xsymbol,
            toString(dQuote(names(expectedCols), FALSE)))
    }

    return(invisible(tokens))
}
