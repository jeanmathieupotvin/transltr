#' @export
translate <- function(..., concat = " ", source_key = "en") {
    trans <- translator_get()
    lang  <- language_get()
    return(
        trans$translate(...,
            key        = lang,
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
