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
}
