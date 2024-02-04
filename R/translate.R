#' @export
translate <- function(
    text = character(),
    ...,
    concat  = " ",
    lang    = getLanguage(),
    srcLang = getSrcLanguage(),
    parse   = FALSE)
{
    if (!isSingleLgl(parse)) {
        halt("'parse' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').")
    }
    if (parse) {
        return(
            list(
                text    = text,
                concat  = concat,
                lang    = substitute(lang),
                srcLang = srcLang))
    }

    return(.NotYetImplemented())
}

isTranslateCall <- function(x, ...) {
    return(UseMethod("isTranslateCall"))
}

#' @export
isTranslateCall.SrcExpr <- function(x, ...) {
    x <- x$expr
    return(NextMethod())
}

#' @export
isTranslateCall.default <- function(x, ...) {
    # Calling a function with backticks or quotes
    # is syntactically valid. transltr::translate
    # and transltr::"translate" are two distinct
    # calls because translate becomes an argument
    # of `::`() as a name or a string.
    return(
        is.call(x) && (
            identical(x[[1L]], quote(translate)) ||
            identical(x[[1L]], quote(transltr::translate)) ||
            identical(x[[1L]], quote(transltr::"translate"))))
}

parseTranslateCall <- function(cl = call(), sloc = NULL) {
    # This prints an error formatted like below.
    #   Error: <...>: while evaluating arguments passed to 'translate()'.
    #    Message : <...>
    #    Location: <...>
    .onCond <- function(c) {
        info <- formatNamedValues(
            Message  = conditionMessage(c),
            Location = format(sloc))

        halt(
            "while evaluating arguments passed to 'translate()'.\n%s",
            paste0(info, collapse = "\n"),
            caller = "parseTranslateCall")
    }

    if (!isTranslateCall(cl)) {
        halt("argment 'cl' is not a call to 'translate()'.")
    }

    cl$parse <- TRUE
    cl <- match.call(transltr::translate, cl, expand.dots = FALSE)
    cl[[1L]] <- call("::", quote(transltr), quote(translate))

    return(
        tryCatch(
            eval(cl),
            error   = .onCond,
            warning = .onCond))
}
