stops <- function(...) {
    stop(..., call. = FALSE)
}

stopf <- function(fmt = "", ...) {
    stops(sprintf(fmt, ...))
}
