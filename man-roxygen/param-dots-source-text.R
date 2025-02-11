#' @param ... Any number of vectors containing [atomic] elements. Each vector
#'   is normalized as a paragraph.
#'
#'   * Elements are coerced to character values.
#'   * NA values and empty strings are discarded.
#'   * Multi-line strings are supported and encouraged. Blank lines are
#'     interpreted (two or more newline characters) as paragraph separators.
