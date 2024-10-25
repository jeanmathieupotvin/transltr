#' Internal Constants
#'
#' @description
#' These constants are used internally by [`transltr`][transltr]. They ensure
#' consistency across its features. Their prefix states their underlying type.
#'
#' **Users should never interact with them.**
#'
#' @name constants
#' @rdname constants
NULL

#' @format * `.__LGL_DEBUG_FLAG` is a logical value always equal to `FALSE`.
#'   It is only ever (temporarily) set equal to `TRUE` in unit tests via
#'   [testthat::with_mocked_bindings()]. It is used to force errors that
#'   are hard to test with usual methods.
#' @rdname constants
#' @keywords internal
.__LGL_DEBUG_FLAG <- FALSE

#' @format * `.__STR_EMPTY_OBJ` is a character string equal to `"<none>"`. It
#'   standardizes the format of empty objects.
#' @rdname constants
#' @keywords internal
.__STR_EMPTY_OBJ <- "<none>"

#' @format * `.__STR_UNDEFINED` is a character string equal to `"<unset>"`. It
#'   signals that a field is not yet set, and may change how empty strings are
#'   formatted.
#' @rdname constants
#' @keywords internal
.__STR_UNDEFINED <- "<unset>"

#' @format * `.__STR_NO_NAME` is a character string equal to `"<nokey>"`. It
#'   is (sometimes) used when formatting empty names.
#' @rdname constants
#' @keywords internal
.__STR_NO_NAME <- "<nokey>"

#' @format * `.__STR_FORMAL_CONCAT_DEFAULT` is a character string equal to `" "`.
#'   It matches the value of formal argument `concat` of [translate()].
#' @rdname constants
#' @keywords internal
.__STR_FORMAL_CONCAT_DEFAULT <- " "

#' @format * `.__STR_FORMAL_KEY_DEFAULT` is a character string equal to `"en"`.
#'   It matches the value of formal argument `source_key` of [translate()].
#' @rdname constants
#' @keywords internal
.__STR_FORMAL_KEY_DEFAULT <- "en"

#' @format * `.__CHR_EXCLUDED_NS` is a character vector containing elements
#'   `"base"`, and `"transltr"`. These are the names of excluded packages (or
#'   [namespaces][loadNamespace()]) when attempting to determine the scope of
#'   a [`Translator`][Translator] object. See [translator_set()] and
#'   [translator_get()] for more information.
#' @rdname constants
#' @keywords internal
.__CHR_EXCLUDED_NS <- c("base", "transltr")
