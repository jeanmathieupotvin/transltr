.onLoad <- function(libname, pkgname) {
    options(
        # Default path to the main Exported Translator file.
    if (is.null(getOption("transltr.verbose", NULL))) {
        # Should functions report basic information by default?
        options(transltr.verbose = TRUE)
    }

    return(invisible())
}

# Suppress R CMD check note.
#' @importFrom R6 R6Class
NULL
