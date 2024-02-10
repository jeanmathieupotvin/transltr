strhash <- function(x = character()) {
    return(vapply1c(x, \(x) sodium::bin2hex(sodium::sha256(charToRaw(x)))))
}

strsanitize <- function(x = character()) {
    return(gsub("[\n\t\r ]+", " ", x))
}

strpad <- function(x = character(), where = c("right", "left")) {
    if (!is.character(x)) {
        halt("'x' must be a character.")
    }

    where   <- .match.arg(where)
    nchars  <- nchar(x)
    padding <- strrep(" ", max(nchars) - nchars)

    return(
        switch(where,
            left  = sprintf("%s%s", padding, x),
            right = sprintf("%s%s", x, padding)))
}

.strwrap <- function(x, indent = 0L, width = getOption("width", 80L)) {
    if (!isSingleIntInRange(indent, 0L)) {
        halt("'indent' must be a non-NA integer value of length 1 greater than or equal to 0.")
    }
    if (!isSingleIntInRange(width, 1L)) {
        halt("'width' must be a non-NA integer value of length 1 strictly greater than 0.")
    }

    return(
        strwrap(x,
            initial = "\n",
            indent  = indent + 1L,
            exdent  = indent + 1L,
            width   = width))
}
