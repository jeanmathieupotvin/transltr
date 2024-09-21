#' Create Tokens
#'
#' Helper classes to represent smallest units of meaningful data. A
#' [`Token`][Token] object has a given `type` and an optional `subtype` that
#' may be used to parse the underlying `value`.
#'
#' @param type Any \R object identifying what type of data `value` holds.
#'
#' @param value Any \R object. [tsf_block_line_token()] expects a character
#'   string representing a source line extracted from a
#'   [Translations Source File][Translations Source Files].
#'
#' @param ... Further arguments. This is useful when extending class
#'   [`Token`][Token].
#'
#' @param .super An optional character vector of super-class names. This
#'   is useful when extending class [`Token`][Token].
#'
#' @param subtype An optional character string. If left as `NULL`, it is
#'     automatically set equal to `type` for convenience.
#'
#' @returns
#' [token()] returns a named list of class `Token` containing the values of
#' arguments `type` and `value`. It will further contain fields that were
#' passed to `...`.
#'
#' [tsf_block_line_token()] returns a named list of class `SrcBlockLineToken`
#' containing the values of argument `type`, `value`, and `subtype`.
#'
#' @seealso [tokenize_tsf_block_v1()],
#'   [tokenize_tsf_block_line_v1()]
#'
#' @rdname class-token
#' @keywords internal
token <- function(type = NULL, value = NULL, ..., .super = NULL) {
    return(
        structure(
            list(type = type, value = value, ...),
            class = c(.super, "Token", "list")))
}

#' @usage
#' tsf_block_line_token <- function(
#'   type = c(
#'     "NULL",
#'     "TITLE_HASH",
#'     "TITLE_KEY_SRC",
#'     "TITLE_KEY_TXT",
#'     "LOC_SRC_PATH",
#'     "LOC_SRC_RNG",
#'     "TXT"),
#'   value   = "",
#'   subtype = NULL
#' )
#' @rdname class-token
#' @keywords internal
tsf_block_line_token <- function(
    type = c(
        "NULL",
        "TITLE_HASH",
        "TITLE_KEY_SRC",
        "TITLE_KEY_TXT",
        "LOC_SRC_PATH",
        "LOC_SRC_RNG",
        "TXT"),
    value   = "",
    subtype = NULL)
{
    subtype <- subtype %||% type

    assert_arg(type, TRUE)
    assert_chr1(value, TRUE)
    assert_chr1(subtype, TRUE)

    return(
        token(
            type,
            value,
            subtype = subtype,
            .super  = "SrcBlockLineToken"))
}
