#' Get or Set Language
#'
#' Get or set the current language of [`transltr`][transltr]. It is registered
#' as an environment variable named `TRANSLTR_LANGUAGE`.
#'
#' [language_set()] leaves the underlying locale as is. To change an \R
#' session's locale, use [Sys.setlocale()] or [Sys.setLanguage()] instead.
#' See below for more information.
#'
#' @template param-lang
#'
#' @returns
#' [language_set()] returns `NULL` invisibly. It is used for its side-effect
#' of setting environment variable `TRANSLTR_LANGUAGE`.
#'
#' [language_get()] returns a character string (possibly empty). It corresponds
#' to the value of environment variable `TRANSLTR_LANGUAGE`.
#'
#' @section Locales versus languages:
#' A [locale](https://en.wikipedia.org/wiki/Locale_(computer_software)) is a
#' set of multiple low-level settings that relate to the user's language and
#' region. The *language* itself is just one parameter among many others.
#'
#' Modifying a locale on-the-fly *can* be considered risky in some situations.
#' It may not be the optimal solution for merely changing textual representations
#' of a program or an application at runtime, as it may introduce unintended
#' changes and induce subtle bugs that are harder to fix.
#'
#' Moreover, it makes sense for some applications and/or programs such as
#' [Shiny applications](https://shiny.posit.co/) to decouple the front-end's
#' current language (what *users* see) from the back-end's locale (what
#' *developers* see). A UI may be displayed in a certain language while keeping
#' logs and \R internal [messages][message()], [warnings][warning()], and
#' [errors][stop()] as is (untranslated).
#'
#' Consequently, the language setting of [`transltr`][transltr] is purposely
#' kept separate from the underlying locale and removes the complexity of
#' having to support many of them. Users can always change both the locale and
#' the `language` parameter of the package. See Examples.
#'
#' @note
#' An environment variable is used because it can be shared among different
#' processes. This matters when using parallel and/or concurrent \R sessions.
#' It can further be shared among direct and transitive dependencies (other
#' packages that rely on [`transltr`][transltr]).
#'
#' @examples
#' ## Change the language parameter (globally).
#' language_set("fr")
#' language_get()  ## Outputs "fr".
#'
#' ## Change both the language parameter and the locale.
#' ## Note that you control how languages are named for language_set(),
#' ## but not for Sys.setLanguage().
#' language_set("fr")
#' Sys.setLanguage("fr-CA")
#'
#' ## Reset settings.
#' language_set(NULL)
#'
#' @rdname language-accessors
#' @export
language_set <- function(lang = "en") {
    if (is.null(lang)) {
        if (!Sys.unsetenv("TRANSLTR_LANGUAGE") || .__LGL_DEBUG_FLAG) {
            stops("failed to unset current language.")
        }

        return(invisible())
    }

    assert_chr1(lang)

    if (!Sys.setenv(TRANSLTR_LANGUAGE = lang) || .__LGL_DEBUG_FLAG) {
        stopf("failed to set language '%s'.", lang)
    }

    return(invisible())
}

#' @rdname language-accessors
#' @export
language_get <- function() {
    # It does not matter whether the environment variable
    # is set equal to "" or truly unset (on some OS only).
    # Both cases leads to the same error. Therefore, there
    # is no need to distinguish these cases with unset = NA.
    return(Sys.getenv("TRANSLTR_LANGUAGE", unset = "", names = FALSE))
}
