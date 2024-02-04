newSrcTranslation <- function(
    text   = "",
    concat = "",
    slang  = "",
    sloc   = NULL)
{
    return(structure(as.list(environment()), class = c("SrcTranslation", "list")))
}

isSrcTranslation <- function(x) {
    return(inherits(x, "SrcTranslation"))
}

asSrcTranslation <- function(x, ...) {
    UseMethod("asSrcTranslation")
}

#' @export
asSrcTranslation.SrcExpr <- function(x, ..., .check = TRUE) {
    if (x$status != "parsed") {
        halt("$status of SrcExpr 'x' must be 'parsed', not '%s'.", x$status)
    }

    args <- parseTranslateCall(x$expr, x$sloc)
    return(newSrcTranslation(args$text, args$concat, args$srcLang, x$sloc))
}

#' @export
format.SrcTranslation <- function(x, indent = 0L, ...) {
    if (!isSingleIntInRange(indent, 0L)) {
        halt("'indent' must be a non-NA integer value of length 1 greater than or equal to 0.")
    }

    x$sloc <- x$sloc %??% SrcLoc()
    text   <- strwrap(
        x$text,
        indent = indent + 2L,
        exdent = indent + 2L,
        width  = getOption("width", 80L))

    return(c(
        formatNamedValues(
            Concat   = sQuote(x$concat, FALSE),
            Language = x$slang %?% "no language specified",
            Location = format(x$sloc),
            Text     = if (all(nzchar(text))) "---" else "<empty>"),
        text))
}

#' @export
print.SrcTranslation <- function(x, ...) {
    cat("<SrcTranslation>", format(x, ...), sep = "\n")
    return(invisible(x))
}

#' @export
asSrcString.SrcTranslation <- function(x, ...) {
    return(.NotYetImplemented())
}
