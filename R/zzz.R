.onLoad <- function(libname, pkgname) {
    options(
        # Default path to the main Exported Translator file.
        transltr.default.path = file.path("inst", "transltr", "_translator.yml"))
}
