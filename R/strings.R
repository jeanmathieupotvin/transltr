hashString <- function(str = "") {
    return(sodium::bin2hex(sodium::sha256(charToRaw(str))))
}

sanitizeString <- function(str = "") {
    return(gsub("[\n\t\r ]+", " ", str))
}

padChr <- function(x = character()) {
    if (!is.character(x)) {
        halt("'x' must be a character.")
    }

    nchars <- nchar(x)
    return(paste0(x, strrep(" ", max(nchars) - nchars)))
}
