#' Normalize Text
#'
#' @description
#' Construct a standardized string from values passed to `...`
#'
#' @template param-dots-source-text
#'
#' @details
#' Input text can written in a variety of ways using single-line and multi-line
#' strings. Values passed to `...` are normalized (to ensure their consistency)
#' and collapsed to a single character string using the standard paragraph
#' separator. The latter is defined as two newline characters (`"\n\n"`).
#'
#'   1. NA values and empty strings are discarded before reducing `...` to a
#'      character string.
#'   2. Whitespaces (tabs, newlines, and repeated spaces) characters are
#'      replaced by a single space. Paragraph separators are preserved.
#'   3. Leading or trailing whitespaces are stripped.
#'
#' @returns
#' A character string, possibly empty.
#'
#' @export
#' @keywords internal
normalize <- function(...) {
    PARAGRAPH_SEP       <- "\n\n"
    PARAGRAPH_SEP_REGEX <- "\n{2,}"

    return(
        # Each vector passed to ... represents a paragraph.
        c(...) |>
        # Step 1. Concatenate all elements into a single
        # string. Discard NA values and empty strings.
        stringi::stri_flatten(
            collapse   = PARAGRAPH_SEP,
            na_empty   = TRUE,
            omit_empty = TRUE) |>
        # Step 2. Split string into a character vector. Each
        # element is a paragraph. Any string of two or more
        # newlines is interpreted as a paragraph separator.
        # This ensures that paragraphs are preserved.
        stringi::stri_split_regex(PARAGRAPH_SEP_REGEX) |>
        _[[1L]] |>
        # Step 3. Replace one or more whitespaces by a space.
        stringi::stri_replace_all_charclass(
            # NOTE: See this Wikipedia page for Unicode
            # property Wspace and what it contains:
            # https://en.wikipedia.org/wiki/Whitespace_character#Unicode.
            pattern     = "\\p{Wspace}",
            replacement = " ",
            merge       = TRUE) |>
        # Step 4. Remove all trailing and leading spaces.
        stringi::stri_trim_both() |>
        # Step 5. Concatenate all paragraphs together
        # using the standard paragraph separator.
        stringi::stri_flatten(
            collapse   = PARAGRAPH_SEP,
            na_empty   = TRUE,
            omit_empty = TRUE)
    )
}
