#' Normalize Text
#'
#' @description
#' [normalize()] constructs a string from single-line and multi-line strings
#' passed to `...` All elements are (implicitly) coerced to character values
#' in the process.
#'
#' @template section-text-normalization
#'
#' @param ... Any number of character vectors.
#'
#' @returns
#' A character string, possibly empty.
#'
#' @keywords internal
normalize <- function(...) {
    dots <- c(...)

    # Replace spaces, tabs, and
    # newlines by a single space.
    strings <- gsub("[ \t\n]+", " ", dots)

    # Ignore leading and trailing empty strings.
    is_nz_pos <- which(nzchar(strings))
    strings   <- strings[seq.int(min(is_nz_pos), max(is_nz_pos))]

    # Empty strings inserted within other (non-empty)
    # strings are interpreted as a paragraph separator.
    # The latter is defined as two line breaks.
    strings[!nzchar(strings)] <- "\n\n"

    # Ensure a character string is returned.
    # Strip leading and trailing whitespaces from it.
    return(gsub("^[ \t\n]+|[ \t\n]+$", "", paste0(strings, collapse = "")))
}
