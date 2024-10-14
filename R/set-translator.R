#' Placeholder
#'
#' Placeholder for future functions [set_translator()] and [get_translator()].
#'
#' @rdname set-translator
#' @export
set_translator <- function(x = translator()) {
    if (!is.null(x) && !is_translator(x)) {
        stops("'x' must be a 'Translator' object.")
    }

    env <- parent.frame()
    attr(env, ".__translator__") <- x
    return(invisible())
}

#' @rdname set-translator
#' @export
get_translator <- function() {
    # sys.nframe() counts call to get_translator(), but
    # excludes global environment. Below, we consider
    # global environment to be part of the stack, but
    # not the call to get_translator(). Therefore,
    # n = sys.nframe() - 1L + 1L = sys.nframe().
    frames      <- c(.GlobalEnv, sys.frames())[seq.int(sys.nframe(), 1L, -1L)]
    trans_chain <- lapply(frames, attr,
        which = ._TRANSLATOR_ATTR_NAME,
        exact = TRUE)

    trans <- unlist(trans_chain, FALSE)[[1L]]
    return(if (is_translator(trans)) trans else NULL)
}

._TRANSLATOR_ATTR_NAME <- ".__translator__"
