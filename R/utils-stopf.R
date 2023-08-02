#' Throw errors
#'
#' Format an error message and throw an error by combining [base::sprintf()]
#' and [base::stop()].
#'
#' @param .concat a character string used to concatenate values passed to `...`
#'
#' @param .args a list containing values to be used to format an error message.
#'   Passed to argument `...` of [base::sprintf()].
#'
#' @param ... values to be assembled into a single error message. They may
#'   contain conversion specifications. See Details of [base::sprintf()].
#'
#' @details
#' Since they can be be validated at edit time using appropriate unit tests,
#' arguments `.concat` and `.args` are **not** validated at runtime.
#'
#' Values passed to `...` are first combined into a single vector with
#' [base::c()], then coerced to a character implicitly by [base::paste0()]
#' before being concatenated.
#'
#' @returns
#' Nothing. [stopf()] is used for its side-effect.
#'
#' @examples
#' \dontrun{
#'
#' stopf("this is a %s", "error", "%s.", .args = list("useful", "message"))
#' ## Error: this is a useful error message.
#' }
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @family internal tools
#'
#' @rdname utils-stopf
#'
#' @keywords internal
stopf <- function(..., .concat = " ", .args = list()) {
    # collapse works on values within
    # vectors so c() is required here.
    fmt <- paste0(c(...), collapse = .concat)

    # Not calling return() here because stop()
    # prevents such a call before it happens.
    stop(do.call(sprintf, c(fmt = fmt, .args)), call. = FALSE)
}

