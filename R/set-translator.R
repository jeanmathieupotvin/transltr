# FIXME: the following implementation are temporary. Not that they are bad,
# but they do not incorporate a namespacing concept. There is a single (global)
# chain of Translator objects which breaks R usual encapsulation mechanisms
# for packages. Instead, it should handle one chain per package and default to
# a global chain when not in "package mode". This will be fixed later, as the
# current focus is on class Translator and function translate().


#' @export
set_translator <- function(x = translator()) {
    if (!is.null(x) && !is_translator(x)) {
        stops("'x' must be a 'Translator' object.")
    }

    env <- parent.frame()
    attr(env, .__OBJ_TRANSLATOR) <- x
    return(invisible())
}

#' @export
get_translator <- function() {
    # sys.nframe() counts call to get_translator(), but
    # excludes global environment. Below, we consider
    # global environment to be part of the stack, but
    # not the call to get_translator(). Therefore,
    # n = sys.nframe() - 1L + 1L = sys.nframe().
    frames      <- c(.GlobalEnv, sys.frames())[seq.int(sys.nframe(), 1L, -1L)]
    trans_chain <- lapply(frames, attr,
        which = .__TRANSLATOR,
        exact = TRUE)

    trans <- unlist(trans_chain, FALSE)[[1L]]
    return(if (is_translator(trans)) trans else NULL)
}
