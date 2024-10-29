#' Translate Text
#'
#' Translate source text to a language. This is an high-level interface to
#' [`Translator$translate()`][Translator].
#'
#' See [translator_set()] for more information on scopes.
#'
#' @param ... Any number of character vectors. The source text to translate.
#'
#' @template param-lang
#'
#' @template param-scope
#'
#' @template param-concat
#'
#' @template param-source-lang
#'
#' @note
#' It is recommended to **always** use [translate()] instead of
#' [`Translator$translate()`][Translator]. The latter is not detected by
#' [find_translations()], while the former is.
#'
#' @seealso
#'   [`Translator`][Translator],
#'   [translator_set()],
#'   [language_set()]
#'
#' @examples
#' ## Create a Translator. This would normally be done
#' ## automatically by find_source() or read_translations().
#' my_translator <- translator(block("en", en = "Hello!", es = "¡Hola!"))
#'
#' ## Register it (implicitly, under 'global' scope).
#' translator_set(my_translator)
#'
#' ## Set current language.
#' language_set("es")
#'
#' ## Request translations.
#' translate("Hello!") ## Outputs "¡Hola!".
#' translate("Hello!", lang = "en") ## Outputs "Hello!".
#'
#' @export
translate <- function(...,
    lang        = language_get(),
    scope       = NULL,
    concat      = " ",
    source_lang = "en")
{
    if (!is_translator(trans <- translator_get(scope))) {
        stopf(
            "no 'Translator' object set for scope '%s'. %s",
            scope %??% translator_scope(),
            "Call 'translator_set()' first.")
    }

    return(
        trans$translate(...,
            lang        = lang,
            concat     = concat,
            source_lang = source_lang))
}

is_translate_call <- function(x, ...) {
    UseMethod("is_translate_call")
}

#' @export
is_translate_call.character <- function(x, ...) {
    return(grepl("^([`'\"]?transltr[`'\"]?::)?[`'\"]?translate[`'\"]?\\((.*?)\\)$", x))
}

match_translate_call <- function(x, ...) {
    UseMethod("match_translate_call")
}

#' @export
match_translate_call.character <- function(x, ...) {
    return(match_translate_call.call(str2lang(x), ...))
}

#' @export
match_translate_call.call <- function(x, ...) {
    # We do not check whether the call object truly calls
    # translate() because this check should be done prior
    # to calling match_translate_call().
    cl        <- match.call(translate, x, expand.dots = FALSE)
    cl$concat <- x$concat %??% .__STR_FORMAL_CONCAT_DEFAULT
    cl$lang   <- x$lang   %??% .__STR_FORMAL_SOURCE_LANG_DEFAULT
    return(cl)
}
