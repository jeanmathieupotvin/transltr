#' Find translations in an expression
#'
#' Extract strings passed to [translate()] in an [expression][base::expression]
#' vector.
#'
#' @param expr an [expression][base::expression]. It can contain
#'   [symbols][base::name],
#'   [calls][base::call],
#'   further [expressions][base::expression], etc. See [base::parse()] for
#'   information.
#'
#' @param env an [environment][base::environment].
#'
#' @details
#' Strings extracted from the same [call][base::call] (`expr`) are coerced
#' to a single [TranslatableString] object.
#'
#' @section Implementation:
#'
#' [getTranslationsFromExpression()] is split into:
#'
#'   1. an outer part, [getTranslationsFromExpression()], and
#'   2. an inner part, [.getTranslationsFromExpression()].
#'
#' The inner part encapsulates the *main* recursion mechanism that loops
#' through expressions, modifying `env` (in place) each time it encounters
#' a [call][base::call] to [translate()]. The outer part initialiazes this
#' mechanism and returns the final output when it is done.
#'
#' @returns
#' A named list (possibly empty) containing [TranslatableString] objects.
#' Names correspond to their signatures.
#'
#' @note
#' The author was initially inspired by the source code of [tools::xgettext()].
#' Many thanks to the original author and to `R Core Team`.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @seealso [TranslatableString],
#'   [tools::xgettext()]
#'
#' @rdname get-translations-from-expresssion
#'
#' @keywords internal
getTranslationsFromExpression <- function(expr = expression()) {
    env <- .getTranslationsFromExpression(expr, new.env(parent = emptyenv()))
    return(as.list(env, sorted = TRUE))
}

#' @rdname get-translations-from-expresssion
.getTranslationsFromExpression <- function(expr = expression(), env = new.env()) {
    if (isCallToTranslate(expr)) {
        string <- as.TranslatableString(newTranslateCall(expr))
        assign(string$signature, string, env)
    }

    if (is.recursive(expr)) {
        for (i in seq_along(expr)) {
            Recall(expr[[i]], env)
        }
    }

    return(invisible(env))
}
