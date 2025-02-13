#' Get or Set Language
#'
#' @description
#' Get or set the current, and source languages.
#'
#' They are registered as environment variables named
#' `TRANSLTR_LANGUAGE`, and `TRANSLTR_SOURCE_LANGUAGE`.
#'
#' @details
#' The language and the source language can always be temporarily changed. See
#' argument `lang` of method [`Translator$translate()`][Translator] for more
#' information.
#'
#' The underlying locale is left as is. To change an \R session's locale,
#' use [Sys.setlocale()] or [Sys.setLanguage()] instead. See below for more
#' information.
#'
#' @template param-lang
#'
#' @returns
#' [language_set()], and [language_source_set()] return `NULL`, invisibly. They
#' are used for their side-effect of setting environment variables
#' `TRANSLTR_LANGUAGE` and `TRANSLTR_SOURCE_LANGUAGE`, respectively.
#'
#' [language_get()] returns a character string. It is the current value of
#' environment variable `TRANSLTR_LANGUAGE`. It is empty if the latter is
#' unset.
#'
#' [language_source_get()] returns a character string. It is the current value
#' of environment variable `TRANSLTR_SOURCE_LANGUAGE`. It returns `"en"` if the
#' latter is unset.
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
#' [errors][stop()] as is.
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
#' # Reset settings.
#' language_source_set(NULL)
#' language_set(NULL)
#'
#' # Source language has a default value.
#' language_source_get()  ## Outputs "en"
#'
#' @rdname language
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

#' @rdname language
#' @export
language_get <- function() {
    # It does not matter whether the environment variable
    # is set equal to "" or truly unset (on some OS only).
    # Both cases leads to the same error. Therefore, there
    # is no need to distinguish these cases with unset = NA.
    return(Sys.getenv("TRANSLTR_LANGUAGE", unset = "", names = FALSE))
}

#' @rdname language
#' @export
language_source_set <- function(lang = "en") {
    if (is.null(lang)) {
        if (!Sys.unsetenv("TRANSLTR_SOURCE_LANGUAGE") || .__LGL_DEBUG_FLAG) {
            stopf(
                "failed to unset current source language '%s'.",
                language_source_get())
        }

        return(invisible())
    }

    assert_chr1(lang)

    if (!Sys.setenv(TRANSLTR_SOURCE_LANGUAGE = lang) || .__LGL_DEBUG_FLAG) {
        stopf("failed to set source language '%s'.", lang)
    }

    return(invisible())
}

#' @rdname language
#' @export
language_source_get <- function() {
    x <- Sys.getenv("TRANSLTR_SOURCE_LANGUAGE", unset = "", names = FALSE)
    return(if (nzchar(x)) x else "en")
}
