classes <- function(x) {
    return(vapply(x, \(el) class(el)[[1L]], NA_character_, USE.NAMES = FALSE))
}

typeofs <- function(x) {
    return(vapply(calls, typeof, NA_character_, USE.NAMES = FALSE))
}
