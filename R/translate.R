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


#' Identify Calls to Function translate()
#'
#' @description
#' Check whether an object is a [`call`][call] to [translate()].
#'
#' This function is meant to be used as a *building block* and does not
#' validate its inputs for maximum efficiency.
#'
#' @details
#' An *implicit* call does not specify the namespace, i.e. `translate()`. It
#' may refer to *any* such function, and may not correspond to [translate()].
#' This depends on the user's intents at compile time, and on the
#' [search][search()] path at runtime.
#'
#' An *explicit* call includes the namespace, i.e. `transltr::translate()`,
#' and there is no ambiguity.
#'
#' \R is sometimes an odd language, and this function covers some unusual use
#' cases. See Examples below. By design, it **does not** detect calls to method
#' [`Translator$translate()`][Translator]. Using the latter is discouraged.
#'
#' @param x Any \R object.
#'
#' @param .strict A non-[NA][base::NA] logical value. Must namespace `transltr`
#'   be explicitly stated via operator `::` in the call?
#'
#' @returns A logical value.
#'
#' @examples
#' ## Typical ways to write source calls.
#' transltr:::is_translate_call(str2lang('translate()'), .strict = FALSE)  ## TRUE
#' transltr:::is_translate_call(str2lang('transltr::translate()'))  ## TRUE
#' transltr:::is_translate_call(str2lang('translate()'))  ## FALSE
#'
#' ## Quotes and backticks are also valid.
#' transltr:::is_translate_call(str2lang('"translate"()'), .strict = FALSE)  ## TRUE
#' transltr:::is_translate_call(str2lang('"translate"()'))  ## FALSE
#'
#' transltr:::is_translate_call(str2lang('`translate`()'), .strict = FALSE)  ## TRUE
#' transltr:::is_translate_call(str2lang('`translate`()'))  ## FALSE
#'
#' transltr:::is_translate_call(str2lang('"transltr"::`translate`()'))  ## TRUE
#' transltr:::is_translate_call(str2lang('`transltr`::"translate"()'))  ## TRUE
#'
#' @rdname is-translate-call
#' @keywords internal
is_translate_call <- function(x, .strict = TRUE) {
    return(
        # x is a call and,
        is.call(x) &&
        switch(class(x1 <- x[[1L]]),
            # it is a call to (any) translate function (not strict), or
            name = !.strict && identical(x1, quote(translate)),
            call = {
                # it embeds a call to operator `::` (strict), and
                identical(x1[[1L]],          quote(`::`)) &&
                # the namespace is transltr, and
                identical(as.name(x1[[2L]]), quote(transltr)) &&
                # the function (name) fetched is translate.
                identical(as.name(x1[[3L]]), quote(translate))
            },
            FALSE))
}
