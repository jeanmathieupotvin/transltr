# NOTE: The contents of this template must be manually copied and pasted into
# the documentation of Translator$translate(). This is because @section cannot
# be used within the documentation of an R6 method.
#
#' @section Text Normalization:
#' [`transltr`][transltr] always normalizes character vectors. Elements passed
#' to `...` are transformed and assembled into a single character string to
#' ensure their consistency. Both single-line and multi-line strings are
#' supported.
#'
#' Character vectors go through these five steps.
#'
#'   1. Whitespaces (tabs, newlines, and repeated spaces) characters are
#'      replaced by a single space.
#'   2. Leading and trailing empty strings are discarded.
#'   3. Empty strings inserted within non-empty elements of `...` are
#'      interpreted as paragraph separators. They are replaced by two
#'      newline characters (`\n\n`).
#'   4. All resulting elements (including paragraph separators) are coerced
#'      into a single character string.
#'   5. Any remaining leading and trailing whitespaces are stripped from the
#'      character string.
