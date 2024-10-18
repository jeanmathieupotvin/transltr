#' Internal Constants
#'
#' These constants are used internally by [`transltr`][transltr]. They ensure
#' consistency across its features. Their prefix states their underlying type.
#'
#' `.__OBJ_TRANSLATOR` gives the name of the attribute that references the
#' latest registered [`Translator`][Translator] object. It is set by
#' [set_translator()] and read by [get_translator()].
#'
#' @note
#' Constants `.__STR_FORMAL_CONCAT_DEFAULT` and `.__STR_FORMAL_KEY_DEFAULT` are
#' respectively equal to the default values of formal arguments `concat` and
#' `source_key` of [translate()].
#'
#' @name constants
#' @rdname constants
NULL

#' @format * `.__STR_EMPTY_OBJ` is a character string equal to `"<none>"`.
#' @rdname constants
#' @keywords internal
.__STR_EMPTY_OBJ <- "<none>"

#' @format * `.__STR_UNDEFINED` is a character string equal to `"<unset>"`.
#' @rdname constants
#' @keywords internal
.__STR_UNDEFINED <- "<unset>"

#' @format * `.__STR_NO_NAME` is a character string equal to `"<nokey>"`.
#' @rdname constants
#' @keywords internal
.__STR_NO_NAME <- "<nokey>"

#' @format * `.__STR_FORMAL_CONCAT_DEFAULT` is a character string equal to `" "`.
#' @rdname constants
#' @keywords internal
.__STR_FORMAL_CONCAT_DEFAULT <- " "

#' @format * `.__STR_FORMAL_KEY_DEFAULT` is a character string equal to `"en"`.
#' @rdname constants
#' @keywords internal
.__STR_FORMAL_KEY_DEFAULT <- "en"

#' @format * `.__OBJ_TRANSLATOR` is a character string equal to `".__translator__"`.
#' @rdname constants
#' @keywords internal
.__OBJ_TRANSLATOR <- ".__translator__"
