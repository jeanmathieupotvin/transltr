# Internal objects -------------------------------------------------------------


#' Internal Cache for Translator Objects
#'
#' An internal cache used by [translator_set()] and [translator_get()] to set
#' or get Translator objects, respectively.
#'
#' @note
#' Translator objects are R6 instances, and these are just fancy environments.
#' Environments have reference semantics in R. Therefore, even if a Translator
#' has multiple bindings (one for the user and one in the cache, typically),
#' changes can be made without re-setting objects over and over again.
#'
#' @noRd
.__translators_cache__ <- new.env(parent = emptyenv())


# Hooks ------------------------------------------------------------------------


.onLoad <- function(libname, pkgname) {
    options(
        # Default path to the main Portable Translator File (PTF).
        transltr.default.path = file.path("inst", "transltr", "_translator.yml"))
}
