#' Tag chararacter strings
#'
#' Wrap character strings with double braces (`{{ }}`) and/or remove them.
#'
#' @param strings Character vector of non-[base::NA][NA] values. Empty elements
#'   are left as is and are not tagged.
#'
#' @param normalize A logical flag. Should existing braces be replaced?
#'   Preceding and trailing outer spaces are removed. Escaped double quotes
#'   are also removed.
#'
#' @param keepDoubleQuotes A logical flag. Keep existing outer double quotes?
#'
#' @returns
#' [tag()] and [untag()] return a character vector of a length equal to the
#' length of argument `strings`.
#'
#' [isTagged()] returns a logical vector of a length equal to the length of
#' argument `strings`. A `TRUE` implies that the underlying element is
#' enclosed by double braces. You may use [tag()] to normalize them.
#'
#' @author Jean-Mathieu Potvin (<jm@@potvin.xyz>)
#'
#' @examples
#' # Strings can be wrapped.
#' identical(tag("test string."), "{{ test string. }}")  ## TRUE
#'
#' # Strings can be unwrapped.
#' identical(tag("{{ test string. }}"), "test string.")  ## TRUE
#'
#' # Tags can be normalized.
#' identical(tag("  {{   test string.   }}   "), "{{ test string. }}")  ## TRUE
#'
#' @export
tag <- function(strings = character(), normalize = TRUE) {
    if (!length(strings)) {
        return(character())
    }

    if (!is.character(strings) || any(is.na(strings))) {
        stopf("TypeError", "`strings` must be a character vector of non-NA values.")
    }
    if (!is.logical(normalize) || length(normalize) != 1L || is.na(normalize)) {
        stopf("TypeError", "`normalize` must be a non-NA integer value.")
    }

    if (normalize) {
        strings <- untag(strings)
    }

    tagged <- paste0(.LTAG, " ", strings, " ", .RTAG)

    # Empty strings are tagged above
    # but we want to leave them as is.
    tagged[!nzchar(strings)] <- ""

    return(tagged)
}

#' @rdname tag
#' @export
untag <- function(strings = character(), keepDoubleQuotes = FALSE) {
    if (!length(strings)) {
        return(character())
    }

    if (!is.character(strings) || any(is.na(strings))) {
        stopf("TypeError", "`strings` must be a character vector of non-NA values.")
    }
    if (!is.logical(keepDoubleQuotes) ||
        length(keepDoubleQuotes) != 1L ||
        is.na(keepDoubleQuotes)) {
        stopf("TypeError", "`keepDoubleQuotes` must be a non-NA integer value.")
    }

    left  <- attr(.LTAG, "regex")
    right <- attr(.RTAG, "regex")
    inner <- gsub(left, "", gsub(right, "", strings))

    if (keepDoubleQuotes) {
        # Identify doubly quoted strings
        # and keep them by readding them.
        quoted        <- startsWith(strings, "\"") & endsWith(strings, "\"")
        inner[quoted] <- dQuote(inner[quoted], FALSE)
    }

    # Empty strings are tagged above
    # but we want to leave them as is.
    inner[!nzchar(strings)] <- ""

    return(inner)
}

#' @rdname tag
#' @export
isTagged <- function(strings = character()) {
    if (!is.character(strings) || !length(strings)) {
        return(FALSE)
    }

    return(
        grepl(attr(.LTAG, "regex"), strings) &
        grepl(attr(.RTAG, "regex"), strings))
}


# Internal constants -----------------------------------------------------------


# This regular expression identifies tags possibly
# surrounded by spaces and/or double quotes.
#
# ^      : matches the beginning of a string;
# $      : matches the end of a string;
# \"?    : matches a possibly missing double string character;
# [ \t]* : matches space(s) and tab(s) ([ \t]) zero or multiple times (*);
# \\{\\{ : matches {{ literally;
# \\}\\} : matches }} literally.
.LTAG <- structure("{{", regex = "^\"?[ \t]*\\{\\{[ \t]*")
.RTAG <- structure("}}", regex = "[ \t]*\\}\\}[ \t]*\"?$")
