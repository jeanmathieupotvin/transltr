#' Translate Text
#'
#' Translate source text.
#'
#' @details
#' It is strongly recommended to always include the namespace when using
#' [translate()], i.e. `transltr::translate()`. Doing so ensures that there
#' will be no ambiguity at runtime. See argument `strict` of [find_source()]
#' for additional information.
#'
#' @param tr A [`Translator`][Translator] object.
#'
#' @template param-dots-source-text
#'
#' @template param-lang
#'
#' @template param-concat
#'
#' @template param-source-lang-no-example
#'
#' @returns
#' A character string, or `NULL` if the underlying translation is unavailable.
#'
#' @seealso
#' [`Translator`][Translator],
#' [language_set()]
#'
#' @examples
#' # Set source language.
#' language_source_set("en")
#'
#' # Create a Translator object.
#' # This would normally be done automatically
#' # by find_source(), or translator_read().
#' tr <- translator(
#'   id = "test-translator",
#'   en = "English",
#'   fr = "Français",
#'   text(
#'     en = "Hello, world!",
#'     fr = "Bonjour, monde!"),
#'   text(
#'     en = "Farewell, world!",
#'     fr = "Au revoir, monde!"))
#'
#' # Set current language.
#' language_set("fr")
#'
#' # Request translations.
#' translate("Hello, world!")                           ## Outputs "Bonjour, monde!"
#' translate("Farewell, world!", lang = "fr", tr = tr)  ## Outputs "Au revoir, monde!"
#' translate("Hello, world!",    lang = "en", tr = tr)  ## Outputs "Hello, world!"
#'
#' @export
translate <- function(
    ...,
    lang        = language_get(),
    tr          = translator(),
    concat      = constant("concat"),
    source_lang = language_source_get())
{
    if (!is_translator(tr)) {
        stops("'tr' must be a 'Translator' object.")
    }

    return(
        tr$translate(...,
            lang        = lang,
            concat      = concat,
            source_lang = source_lang))
}
