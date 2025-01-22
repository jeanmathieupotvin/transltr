#' Identify Calls to Function translate()
#'
#' @description
#' Check whether an object is a [`call`][call] to [translate()].
#'
#' **Arguments listed below are not explicitly validated for efficiency.**
#'
#' @details
#' An *implicit* call does not specify the namespace, i.e. `translate()`. It
#' may refer to *any* such function and may not correspond to [translate()].
#' This depends on the user's intents at compile time and on the
#' [search][search()] path at runtime.
#'
#' An *explicit* call includes the namespace, i.e. `transltr::translate()`.
#' There is no ambiguity.
#'
#' \R is sometimes an odd language and this function covers some unusual use
#' cases. See Examples below. By design, it **does not** detect calls to method
#' [`Translator$translate()`][Translator]. Using the latter is discouraged.
#'
#' @param x Any \R object.
#'
#' @param strict A non-[NA][base::NA] logical value. Must the `transltr`
#'   namespace be explicitly stated (with operator `::`) in the call? See
#'   Details.
#'
#' @returns A logical value.
#'
#' @rdname translate-is-call
#' @keywords internal
is_translate_call <- function(x, strict = TRUE) {
    return(
        # x is a call and,
        is.call(x) &&
        switch(class(x1 <- x[[1L]]),
            # it is a call to (any) translate function (not strict), or
            name = !strict && identical(x1, quote(translate)),
            call = {
                # it embeds a call to operator `::` (strict), and
                identical(x1[[1L]], quote(`::`)) &&
                # the namespace is transltr, and
                identical(as.name(x1[[2L]]), quote(transltr)) &&
                # the function (name) fetched is translate.
                identical(as.name(x1[[3L]]), quote(translate))
            },
            FALSE))
}
