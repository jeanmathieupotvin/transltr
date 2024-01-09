newTranslateCall <- function(
    text = character(),
    ...,
    concat  = getDefaultConcat(),
    lang    = getCurrentLang(),
    srcLang = getDefaultSrcLang())
{
    args <- pairlist(
        text    = substitute(text),
        concat  = substitute(concat),
        lang    = substitute(lang),
        srcLang = substitute(srcLang),
        ...)

    cl <- as.call(c(quote(transltr::translate), args))

    # Match arguments to separate named values
    # passed to ... from formal named arguments.
    cl <- match.call(transltr::translate, cl, expand.dots = FALSE)

    class(cl) <- c("TranslateCall", class(cl))
    return(cl)
}

isTranslateCall <- function(x) {
    return(inherits(x, "TranslateCall"))
}

asTranslateCall <- function(x, ...) {
    return(UseMethod("asTranslateCall"))
}

#' @export
asTranslateCall.call <- function(x, ...) {
    cl <- match.call(transltr::translate, x, expand.dots = FALSE)
    class(cl) <- c("TranslateCall", class(cl))
    return(cl)
}

isCallToTranslate <- function(x, ...) {
    return(UseMethod("isCallToTranslate"))
}

#' @export
isCallToTranslate.SrcExpr <- function(x, ...) {
    x <- x$expr
    return(NextMethod())
}

#' @export
isCallToTranslate.default <- function(x, ...) {
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
