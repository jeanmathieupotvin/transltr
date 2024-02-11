#' @export
getLanguage <- function() {
    return(Sys.getenv("TRANSLTR_LANG", "en", FALSE))
}

#' @export
setLanguage <- function(lang = "en") {
    assertNonEmptyString(lang)
    return(Sys.setenv(TRANSLTR_LANG = lang))
}

#' @export
getSrcLanguage <- function() {
    return(Sys.getenv("TRANSLTR_SRCLANG", "en", FALSE))
}

#' @export
setSrcLanguage <- function(lang = "en") {
    assertNonEmptyString(lang)
    return(Sys.setenv(TRANSLTR_SRCLANG = lang))
}
