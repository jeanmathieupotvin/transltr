translate <- function(..., concat = " ", source_key = "en") {
    trans <- get_translator()
    return(trans$translate(..., concat = concat, source_key = source_key))
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
    cl$concat <- x$concat %??% ._TRANSLATE_FORMAL_CONCAT
    cl$key    <- x$key    %??% ._TRANSLATE_FORMAL_KEY
    return(cl)
}

# Not passed to translate() because
# internal constants should be kept
# hidden from users. However, these
# constants must be in sync with
# formal args of translate().
._TRANSLATE_FORMAL_CONCAT <- eval(formals(translate)$concat)
._TRANSLATE_FORMAL_KEY    <- eval(formals(translate)$key)
