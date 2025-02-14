.onLoad <- function(libname, pkgname) {
    if (is.null(getOption("transltr.path", NULL))) {
        # Default path to the main Exported Translator file.
        options(transltr.path = file.path("inst", "transltr", "_translator.yml"))
    }
    if (is.null(getOption("transltr.verbose", NULL))) {
        # Should functions report basic information by default?
        options(transltr.verbose = TRUE)
    }

    return(invisible())
}
