hashString <- function(str = "") {
    return(sodium::bin2hex(sodium::sha256(charToRaw(str))))
}

sanitizeString <- function(str = "") {
    return(gsub("[\n\t\r ]+", " ", str))
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
