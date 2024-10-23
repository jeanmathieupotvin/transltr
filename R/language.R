#' Current Language
#'
#' Get or set the current language **key** setting of [`transltr`][transltr].
#' It is registered as an environment variable named `TRANSLTR_LANGUAGE`.
#'
#' [language_set()] leaves the underlying locale as is. To change an \R
#' session's locale, use [Sys.setlocale()] instead. Users can safely use
#' both simultaneously if required. See below for more information.
#'
#' @template param-key
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
#' [errors][stop()] as is (in the source language).
#'
#' Consequently, the language setting of [`transltr`][transltr] is purposely
#' kept separate from the underlying locale and removes the complexity of
#' having to support many of them. You may still do so while using the package.
#'
#' @note
#' An environment variable is used because it can be shared among different
#' processes. This matters when using parallel and/or concurrent \R sessions.
#'
#' @rdname language
#' @export
language_set <- function(key = "en") {
    if (is.null(key)) {
        if (!Sys.unsetenv("TRANSLTR_LANGUAGE") || .__LGL_DEBUG_FLAG) {
            stops("failed to unset current language key.")
        }

        return(invisible())
    }

    assert_chr1(key)

    if (!Sys.setenv(TRANSLTR_LANGUAGE = key) || .__LGL_DEBUG_FLAG) {
        stopf("failed to set language key '%s'.", key)
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
