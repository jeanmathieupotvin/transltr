#' Throw errors
#'
#' @description
#' This function is **experimental**.
#'
#' [stopf()] combines [`sprintf()`][base::sprintf()] and
#' [`stop()`][base::stop()]. It further replaces usual `Error: ...`
#' prefix by an informative error `type` such as `TypeError: ...` .
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
