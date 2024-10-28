#' Translate Text
#'
#' Translate source text to a language given by `key`. This is an high-level
#' interface to [`Translator$translate()`][Translator].
#'
#' @param scope A `NULL`, or an optional non-empty and non-[NA][base::NA]
#'   character string. See [translator_set()] for more information.
#'
#' @template param-dots-source-text
#'
#' @template param-key
#'
#' @template param-source-key
#'
#' @template param-concat
#'
#' @note
#' It is recommended to **always** use [translate()] instead of
#' [`Translator$translate()`][Translator]. The latter is not detected by
#' [find_translations()], while the former is.
#'
#' @seealso [`Translator`][Translator],
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
#' translate("Hello!", key = "en") ## Outputs "Hello!".
#'
#' @export
translate <- function(...,
    key        = language_get(),
    scope      = NULL,
    source_key = "en",
    concat     = " ")
{
    if (!is_translator(trans <- translator_get(scope))) {
        stopf(
            "no 'Translator' object set for scope '%s'. %s",
            scope %??% translator_scope(),
            "Call 'translator_set()' first.")
    }

    return(
        trans$translate(...,
            key        = key,
            concat     = concat,
            source_key = source_key))
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
    cl$key    <- x$key    %??% .__STR_FORMAL_KEY_DEFAULT
    return(cl)
}
