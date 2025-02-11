# NOTE: The contents of this template must be manually copied and pasted into
# the documentation of Translator$translate(). This is a limitation of rxoygen2.
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
