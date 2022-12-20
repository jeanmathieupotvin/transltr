map <- function(fun, ..., moreArgs = list()) {
    return(.mapply(fun, list(...), moreArgs))
}

vapply1i <- function(x, fun, ..., useNames = FALSE) {
    return(vapply(x, fun, NA_integer_, ..., USE.NAMES = useNames))
}

vapply1c <- function(x, fun, ..., useNames = FALSE) {
    return(vapply(x, fun, NA_character_, ..., USE.NAMES = useNames))
}
