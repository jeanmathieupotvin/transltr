isString <- function(x) {
    return(is.character(x) && length(x) == 1L && !is.na(x))
}

isNonEmptyString <- function(x) {
    return(isString(x) && nzchar(x))
}

isSingleLgl <- function(x) {
    return(is.logical(x) && length(x) == 1L && !is.na(x))
}

isSingleIntInRange <- function(x, min = -max, max = .Machine$integer.max) {
    if (min > max) {
        halt("'min' must be lower than or equal to 'max'.")
    }

    return(is.integer(x) && length(x) == 1L && !is.na(x) && x >= min && x <= max)
}

isSingleDblInRange <- function(
    x,
    min = .Machine$double.xmin,
    max = .Machine$double.xmax)
{
    if (min > max) {
        halt("'min' must be lower than or equal to 'max'.")
    }

    return(is.double(x) && length(x) == 1L && !is.na(x) && x >= min && x <= max)
}
