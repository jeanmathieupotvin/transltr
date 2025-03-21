#' Normalize Text
#'
#' @description
#' Construct a standardized string from values passed to `...`.
#'
#' @template param-dots-source-text
#'
#' @param x A character vector.
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
#' Constants returned by [paragraph_std_template()] cannot be changed.
#'
#' @returns
#' [normalize()] returns a character string, possibly empty.
#'
#' [paragraph_std_template()] returns a named character vector of length 3
#' containing the following elements.
#'
#' \describe{
#'   \item{`sep`}{The standard paragraph separator (two newline characters).
#'     It is visually equivalent to a blank line.}
#'   \item{`regex`}{A regular expression used to detect paragraphs. Any
#'     substring of at least 2 consecutive newline characters are treated as
#'     separators. [normalize()] replaces them by `sep`.}
#'   \item{`escape`}{What to show instead of `sep` in console outputs.}
#' }
#'
#' [escape_std_paragraph_sep()] returns a character vector identical to `x`
#' (with escaped paragraph separators). Names are preserved, but not other
#' attributes.
#'
#' @export
#' @keywords internal
normalize <- function(...) {
    if (!...length() || ...length() == 1L && is.null(..1)) {
        return("")
    }
    if (!is.character(dots <- c(...))) {
        stops("values passed to '...' must all be character vectors.")
    }

    p <- paragraph_std_template()

    return(
        # Each vector passed to ... represents a paragraph.
        # Each element of each vector passed to ... may contain further
        # paragraphs by using any separator that matches p[["regex"]].
        # It will be normalized to p[["sep"]].
        dots |>
        # Step 1. Concatenate all elements into a single
        # string. Discard NA values and empty strings.
        stringi::stri_flatten(
            collapse   = p[["sep"]],
            na_empty   = TRUE,
            omit_empty = TRUE) |>
        # Step 2. Split string into a character vector. Each
        # element is a paragraph. Any string of two or more
        # newlines is interpreted as a paragraph separator.
        # This ensures that paragraphs are preserved.
        stringi::stri_split_regex(p[["regex"]]) |>
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
            collapse   = p[["sep"]],
            na_empty   = TRUE,
            omit_empty = TRUE)
    )
}

#' @rdname normalize
#' @export
#' @keywords internal
paragraph_std_template <- function() {
    return(c(sep = "\n\n", regex = "\n{2,}", escape = "<empty-line>"))
}

#' @rdname normalize
#' @export
#' @keywords internal
escape_std_paragraph_sep <- function(x = character()) {
    assert_chr(x, TRUE)

    p <- paragraph_std_template()
    x_esc <- stringi::stri_replace_all_regex(x, p[["regex"]], p[["escape"]])
    names(x_esc) <- names(x)
    return(x_esc)
}
