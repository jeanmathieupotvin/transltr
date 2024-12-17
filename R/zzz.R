.onLoad <- function(libname, pkgname) {
    options(
        # Default path to the main Portable Translator File (PTF).
        transltr.default.path = file.path("inst", "transltr", "_translator.yml"))
}
