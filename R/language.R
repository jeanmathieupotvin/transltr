#' Get or Set Language
#'
#' Get or set the current language and source language. They are registered as
#' environment variables, and respectively named `TRANSLTR_LANGUAGE`, and
#' `TRANSLTR_SOURCE_LANGUAGE`.
#'
#' [language_set()] leaves the underlying locale as is. To change an \R
#' session's locale, use [Sys.setlocale()] or [Sys.setLanguage()] instead.
#' See below for more information.
#'
#' [language_source_set()] sets the language of the source text (globally).
#'
#' Both the language and the source language can be changed locally. See
#' [translate()] for more information.
#'
#' @template param-lang
#'
#' @template param-source-lang-no-example
#'
#' @returns
#' [language_set()], and [language_source_set()] return `NULL`, invisibly. They
#' are used for their side-effect of setting environment variables
#' `TRANSLTR_LANGUAGE` and `TRANSLTR_SOURCE_LANGUAGE`, respectively.
#'
#' [language_get()], and [language_source_get()] return a character string
#' corresponding to the value of environment variables `TRANSLTR_LANGUAGE`
#' and `TRANSLTR_SOURCE_LANGUAGE`, respectively.
#'
#'   * [language_source_get()] returns a default value equal to `"en"` if
#'     `TRANSLTR_SOURCE_LANGUAGE` unset.
#'   * [language_get()] never returns a default value.
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
#' Environment variables are used because they can be shared among different
#' processes. This matters when using parallel and/or concurrent \R sessions.
#' It can further be shared among direct and transitive dependencies (other
#' packages that rely on [`transltr`][transltr]).
#'
#' @examples
#' # Change the language parameters (globally).
#' language_source_set("en")
#' language_set("fr")
#'
#' language_source_get()  ## Outputs "en"
#' language_get()         ## Outputs "fr"
#'
#' # Change both the language parameter and the locale.
#' # Note that while users control how languages are named
#' # for language_set(), they do not for Sys.setLanguage().
#' language_set("fr")
#' Sys.setLanguage("fr-CA")
#'
#' ## Reset settings.
#' language_source_set(NULL)
#' language_set(NULL)
#'
#' @rdname language-accessors
#' @export
language_set <- function(lang = "en") {
    if (is.null(lang)) {
        if (!Sys.unsetenv("TRANSLTR_LANGUAGE") || .__LGL_DEBUG_FLAG) {
            stopf("failed to unset current language '%s'.", language_get())
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

#' @rdname language-accessors
#' @export
language_source_set <- function(source_lang = "en") {
    if (is.null(source_lang)) {
        if (!Sys.unsetenv("TRANSLTR_SOURCE_LANGUAGE") || .__LGL_DEBUG_FLAG) {
            stopf(
                "failed to unset current source language '%s'.",
                language_source_get())
        }

        return(invisible())
    }

    assert_chr1(source_lang)

    if (!Sys.setenv(TRANSLTR_SOURCE_LANGUAGE = source_lang) || .__LGL_DEBUG_FLAG) {
        stopf("failed to set source language '%s'.", source_lang)
    }

    return(invisible())
}

#' @rdname language-accessors
#' @export
language_source_get <- function() {
    return(Sys.getenv("TRANSLTR_SOURCE_LANGUAGE", unset = "en", names = FALSE))
}
