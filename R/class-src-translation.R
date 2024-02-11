newSrcTranslation <- function(
    text   = "",
    concat = "",
    slang  = "",
    sloc   = NULL)
{
    return(
        structure(
            as.list(environment()),
            class = c("SrcTranslation", "list")))
}

isSrcTranslation <- function(x) {
    return(inherits(x, "SrcTranslation"))
}

asSrcTranslation <- function(x, ...) {
    UseMethod("asSrcTranslation")
}

#' @export
asSrcTranslation.SrcExpr <- function(x, ...) {
    if (x$status != "parsed") {
        halt("$status of SrcExpr 'x' must be 'parsed', not '%s'.", x$status)
    }

    args <- parseTranslateCall(x$expr, x$sloc)
    return(newSrcTranslation(args$text, args$concat, args$slang, x$sloc))
}

#' @export
format.SrcTranslation <- function(x, indent = 0L, ...) {
    sloc  <- x$sloc %??% SrcLoc()
    slang <- x$slang %?% "<none>"
    text  <- .strwrap(strsanitize(x$text %?% "<empty>"), indent)
    named <- formatNamedValues(
        Concat   = sQuote(x$concat, FALSE),
        Language = slang,
        Location = format(x$sloc))

    return(c(named, text))
}

#' @export
print.SrcTranslation <- function(x, ...) {
    cat("<SrcTranslation>", format(x, ...), sep = "\n")
    return(invisible(x))
}
