#' Throw errors
#'
#' Combine [`sprintf()`][base::sprintf()] and [`stop()`][base::stop()] in
#' a single call.
#'
#' It further introduces new informative labels for errors. They are always
#' included in error messages:
#'
#' ```
#' Error: <TypeError>: ...
#' ```
#'
#' The `<TypeError>` string above is replaced by a `type`. See below for
#' available values.
#'
#' \describe{
#'
#' \item{`InterfaceError`}{
#'   An input or an argument passed to a function do not conform to the
#'   accepted standards of an *external dependency*:
#'
#'   * a function defined in another package,
#'   * a value passed to another software internally, etc.
#'
#' }
#' \item{`LogicError`}{
#'   A function operated incorrectly and/or triggered an unanticipated
#'   situation without terminating abnormally. This (obviously) happened
#'   before triggering a call to [stopf()].
#'
#'   This is usually a type used by *guard clauses* that cover edge cases of a
#'   function.
#' }
#' \item{`TypeError`}{
#'   An input or an argument passed to a function is incompatible with its
#'   expected *shape*:
#'
#'   * its [type][base::typeof()],
#'   * its [class][base::class()],
#'   * its [length][base::length()],
#'   * its [attributes][base::attributes()],
#'   * its boundaries, and
#'   * any other restrictions that depend on the underlying context.
#'
#' }
#' }
#'
#' @param type Character string stating the underlying error's type.
#'
#' @param fmt,... Passed to function [`sprintf()`][base::sprintf()].
#'
#' @returns
#' Nothing. This function is used for its side-effect.
#'
#' @author Jean-Mathieu Potvin (<jm@@potvin.xyz>)
#'
#' @examples
#' \dontrun{
#' stopf("TypeError", "argument must contain exactly %i values.", 7L)
#' # TypeError: argument must contain exactly 7 values.
#' }
#'
#' @keywords internal
stopf <- function(
    type = c("InterfaceError", "LogicError", "TypeError"),
    fmt  = character(1L), ...)
{
    # Construct a new fmt with format Type: fmt.
    type     <- match.arg(type)
    template <- sprintf("%s: %s", type, fmt)

    stop(sprintf(template, ...), call. = FALSE)
}
