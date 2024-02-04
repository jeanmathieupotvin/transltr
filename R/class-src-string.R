SrcString <- function(str = "", lang = "", sloc = NULL) {
    if (!isString(str)) {
        halt("'str' must be a non-NA character of length 1. It can be empty.")
    }
    if (!isString(lang)) {
        halt("'lang' must be a non-NA character of length 1. It can be empty.")
    }
    if (!is.null(sloc) && !isSrcLoc(sloc)) {
        halt("'sloc' must be NULL or a 'SrcLoc' object.")
    }

    return(newSrcString(str, hashString(str), lang, sloc))
}

newSrcString <- function(str = "", hash = "", lang = "", sloc = NULL) {
    return(structure(as.list(environment()), class = c("SrcString", "list")))
}

isSrcString <- function(x) {
    return(inherits(x, "SrcString"))
}

#' @export
format.SrcString <- function(x, indent = 0L, ...) {
    if (!isSingleIntInRange(indent, 0L)) {
        halt("'indent' must be a non-NA integer value of length 1 greater than or equal to 0.")
    }

    x$sloc <- x$sloc %??% SrcLoc()
    str    <- strwrap(
        sanitizeString(x$str),
        indent = indent + 2L,
        exdent = indent + 2L,
        width  = getOption("width", 80L))

    return(c(
        formatNamedValues(
            Hash     = x$hash,
            Language = x$lang %?% "no language specified",
            Location = format(x$sloc),
            String   = if (nzchar(str)) "---" else "<empty>"),
        str))
}

#' @export
print.SrcString <- function(x, ...) {
    cat("<SrcString>", format(x, ...), sep = "\n")
    return(invisible(x))
}

asSrcString <- function(x, ...) {
    UseMethod("asSrcString")
}
