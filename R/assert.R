assertString <- function(x = character(1L)) {
    if (!is.character(x) || length(x) != 1L || !nzchar(x) || anyNA(x)) {
        stopf(
            "TypeError",
            "`%s` must be a non-empty, non-NA character value of length 1.",
            deparse(substitute(x)))
    }

    return(invisible(x))
}

assertNoEmptyStrings <- function(x = character()) {
    if (any(nzchar(x))) {
        stopf(
            "TypeError",
            "`%s` cannot contain empty strings.",
            deparse(substitute(x)))
    }

    return(invisible(x))
}

assertScalarInteger <- function(x = integer(1L)) {
    return(assertScalar(x, "integer", is.integer))
}

assertNonEmptyInteger <- function(x = integer()) {
    return(assertNonEmpty(x, "integer", is.integer))
}

assertNonEmptyCharacter <- function(x = character()) {
    return(assertNonEmpty(x, "character", is.character))
}

assertScalar <- function(x, type, is) {
    if (!is(x) || length(x) != 1L || anyNA(x)) {
        stopf(
            "TypeError",
            "`%s` must be a non-NA %s value of length 1.",
            deparse(substitute(x)), type)
    }

    return(invisible(x))
}

assertNonEmpty <- function(x, type, is) {
    if (!is(x) || !length(x) || anyNA(x)) {
        stopf(
            "TypeError",
            "`%s` must be a non-empty %s vector of non-NA elements.",
            deparse(substitute(x)), type)
    }

    return(invisible(x))
}
