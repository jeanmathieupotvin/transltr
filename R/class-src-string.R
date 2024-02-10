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

    str <- strsanitize(str)
    return(newSrcString(str, strhash(str), lang, sloc))
}

newSrcString <- function(str = "", hash = "", lang = "", sloc = NULL) {
    return(structure(as.list(environment()), class = c("SrcString", "list")))
}

isSrcString <- function(x) {
    return(inherits(x, "SrcString"))
}

#' @export
format.SrcString <- function(x, indent = 0L, ...) {
    sloc  <- x$sloc %??% SrcLoc()
    lang  <- x$lang %?% "<none>"
    str   <- .strwrap(x$str %?% "<empty>", indent)
    named <- formatNamedValues(
        Hash     = x$hash,
        Language = lang,
        Location = format(sloc))

    return(c(named, str))
}

#' @export
print.SrcString <- function(x, ...) {
    cat("<SrcString>", format(x, ...), sep = "\n")
    return(invisible(x))
}

asSrcString <- function(x, ...) {
    UseMethod("asSrcString")
}

#' @export
asSrcString.SrcTranslation <- function(x, ...) {
    return(SrcString(paste0(x$text, collapse = x$concat), x$slang, x$sloc))
}
