#' @export
language_set <- function(key = "en", sys_key = NULL) {
    if (is.null(key)) {
        if (!Sys.unsetenv("TRANSLTR_LANGUAGE")) {
            stops("failed to unset current language key.")
        }

        return(invisible(TRUE))
    }

    assert_chr1(key)

    if (!Sys.setenv(TRANSLTR_LANGUAGE = key)) {
        stopf("failed to set language key '%s'.", key)
    }
    if (!is.null(sys_key)) {
        Sys.setLanguage(sys_key)
    }

    return(invisible(TRUE))
}

#' @export
language_get <- function() {
    key <- Sys.getenv("TRANSLTR_LANGUAGE", unset = "", names = FALSE)
    return(if (is.na(key)) NULL else key)
}
