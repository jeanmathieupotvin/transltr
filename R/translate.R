#' Translate text
#'
#' To be implemented.
#'
#' @param concat to be implemented.
#'
#' @param lang to be implemented.
#'
#' @param translator to be implemented.
#'
#' @param ... to be implemented.
#'
#' @returns
#' Nothing. An error is returned by [base::.NotYetImplemented()].
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @export
translate <- function(..., concat = " ", lang = "en", translator = NULL) {
    return(.NotYetImplemented())
}


# Internal mechanisms ----------------------------------------------------------


#' Manipulate calls to translate()
#'
#' Create [calls][base::call()] to [translate()], identify when this function
#' is called, flag calls as such, and convert them into [TranslatableString]
#' objects.
#'
#' @param x an \R object.
#'
#' @param ... further arguments passed to or from other methods.
#'
#' @details
#' [newTranslateCall()] creates unevaluated function [calls][base::call()] to
#' [translate()]. The default method can be used to construct such calls from
#' arbitrary arguments as if they were passed to [translate()]. It considers
#' `x` to be the first argument passed to `...`
#'
#' [isCallToTranslate()] will not check whether the current state of
#' the [search path][base::search()] yields the correct [translate()]
#' function (when the namespace is omitted).
#'
#' [doEvaluateTranslateDefaultArgs()] is called once whenever \pkg{transltr}
#' is loaded to fetch and evaluate default arguments of [translate()]. They
#' are cached via constant `TRANSLATE_DEFAULT_ARGS`.
#'
#' @returns
#' * [isCallToTranslate()] returns a logical value.
#' * [newTranslateCall()] and related methods return a [TranslateCall] object.
#' * [doEvaluateTranslateDefaultArgs()] returns a named list holding the
#'   default arguments of [translate()]. Dot dot dot is removed.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname translate-internals
#'
#' @keywords internal
isCallToTranslate <- function(x) {
    # Calling a function with backticks or quotes
    # is syntactically valid. transltr::translate
    # and transltr::"translate" are two distinct
    # calls because translate becomes an argument
    # of `::`() as a name or a string. Both cases
    # must be covered.
    return(
        is.call(x) && (
            identical(x[[1L]], quote(translate)) ||
            identical(x[[1L]], quote(transltr::translate)) ||
            identical(x[[1L]], quote(transltr::"translate"))))
}

#' @aliases TranslateCall
#' @rdname translate-internals
newTranslateCall <- function(x, ...) {
    UseMethod("newTranslateCall")
}

#' @rdname translate-internals
#' @export
newTranslateCall.call <- function(x, ...) {
    # Validation is deferred to the caller.
    # We expect a call to translate() here.
    class(x) <- c("TranslateCall", class(x))
    return(x)
}

#' @rdname translate-internals
#' @export
newTranslateCall.default <- function(x, ...) {
    args <- if (missing(x)) list(...) else list(x, ...)
    return(newTranslateCall(do.call(call, c(name = "translate", args))))
}

#' @rdname translate-internals
#' @export
as.TranslatableString.TranslateCall <- function(x, ...) {
    x <- match.call(translate, x, expand.dots = FALSE)

    # dot dot dot is a pairlist and its
    # arguments must be evaluated first.
    # Evaluation happens in the caller's
    # environment.
    dots   <- lapply(x$`...`, eval.parent, n = 2L)
    concat <- x$concat %||% TRANSLATE_DEFAULT_ARGS$concat

    return(TranslatableString$new(dots, concat = concat))
}

#' @usage
#' ## Fetch, evaluate, and cache default arguments of translate()
#' TRANSLATE_DEFAULT_ARGS <- doEvaluateTranslateDefaultArgs()
#'
#' @rdname translate-internals
doEvaluateTranslateDefaultArgs <- function() {
    if (is.null(defaultArgs <- formals(translate))) {
        stopf(
            "translate() must have arguments.",
            "This is an error intended for developers.",
            "Users that see this message should report it.")
    }

    # defaultArgs is a pairlist and its
    # arguments must be evaluated first.
    # Evaluation happens in the caller's
    # environment.
    return(lapply(defaultArgs[names(defaultArgs) != "..."], eval.parent, n = 2L))
}
