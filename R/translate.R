#' @export
translate <- function(
    text = character(),
    ...,
    concat  = getDefaultConcat(),
    lang    = getCurrentLang(),
    srcLang = getDefaultSrcLang())
{
    return(.NotYetImplemented())
}

#' @export
getDefaultConcat <- function() {
    return(
        getOption("transltr.default.concat") %??%
        Sys.getenv("TRANSLTR_DEFAULT_CONCAT", " ", FALSE))
}

#' @export
getCurrentLang <- function() {
    return(getOption("transltr.default.srcLang") %??% "en")
}

#' @export
getDefaultSrcLang <- function() {
    return(
        getOption("transltr.default.srcLang") %??%
        Sys.getenv("TRANSLTR_DEFAULT_SRCLANG", "en", FALSE))
}
