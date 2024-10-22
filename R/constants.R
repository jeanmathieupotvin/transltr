#' Internal Constants
#'
#' @description
#' These constants are used internally by [`transltr`][transltr]. They ensure
#' consistency across its features. Their prefix states their underlying type.
#'
#' **Users should never interact with these constants.**
#'
#' @name constants
#' @rdname constants
NULL

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

#' @format * `.__STR_ATTACHED_DB` is a character string equal to
#'   `"transltr:translators"`. This is the name of the environment
#'   [`transltr`][transltr] attaches to the [search path][search()]
#'   when it is (first) loaded. Registered [`Translator`][Translator]
#'   objects are assigned there.
#' @rdname constants
#' @keywords internal
.__STR_ATTACHED_DB <- "transltr:translators"
