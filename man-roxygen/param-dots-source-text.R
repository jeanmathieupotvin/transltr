#' @param ... Any number of [atomic] vectors.
#'
#'   * Each element is coerced to a character and normalized as a paragraph.
#'   * NA values and empty strings are discarded.
#'   * Multi-line strings are supported and encouraged. Any indentation may
#'     be used. Blank lines are interpreted (two or more newline characters)
#'     as paragraph separators.
