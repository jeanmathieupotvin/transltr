#' Operate on Text
#'
#' @description
#' [text_normalize()] transforms text (possibly) split into multiple character
#' strings or across multiple lines.
#'
#' [text_hash()] maps an arbitrary character string (of any length) to a
#' fixed-length output.
#'
#' @details
#' [text_normalize()] enforces conventions on input text.
#'
#'   1. It concatenates elements passed to `...` into a single character string.
#'   2. It removes leading new lines and/or space characters.
#'   3. It replaces substrings of repeated characters by a single space.
#'   4. It removes space characters that (immediately) follow new line characters.
#'
#' Doing so allows users to either split source text into multiple character
#' strings, or write it as a single multi-line character string formatted in
#' a variety of ways. See Examples.
#'
#' [text_hash()] returns a reproducible hash generated from `.key` and
#' `.text` using the algorithm given by `.hash_algorithm`. What is hashed
#' is the concatenated values of `.key` and `.text`: `.key:.text`.
#'
#' @param ... Any number of character strings, or objects that can be coerced
#'   as such.
#'
#' @param .concat A character string. It is used to concatenate values passed
#'   to `...`. **It is not validated for efficiency.**
#'
#' @param .key A character string. A language key. See class
#'   [`Translator`][Translator] for more information. **It is not
#'   validated for efficiency.**
#'
#' @param .text A character string. **It is not validated for efficiency.**
#'
#' @param hash_algorithm A character string. The algorithm to use when hashing
#'   `.key` and `.text`. It must be a value returned by [get_hash_algorithms()].
#'   **It is not validated for efficiency.**
#'
#' @returns A character string.
#'
#' @examples
#' str1 <- "
#'    Lorem Ipsum is simply dummy text of the printing and typesetting industry.
#'    Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,
#'    when an unknown printer took a galley of type and scrambled it to make a type
#'    specimen book."
#'
#' str2 <- c(
#'    "Lorem Ipsum is simply dummy text of the printing and typesetting industry.",
#'    "Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,",
#'    "when an unknown printer took a galley of type and scrambled it to make a type",
#'    "specimen book.")
#'
#' identical(
#'     text_normalize(str1),
#'     text_normalize(str2, .concat = "\n")  ## TRUE
#'
#' cat(text_normalize(str1), "\n")
#' cat(text_normalize(str2, .concat = "\n"), "\n")
#'
#' @rdname text
#' @keywords internal
text_normalize <- function(..., .concat = " ") {
    str <- c(...) |>
        # Remove leading new lines and/or space characters.
        gsub("^[ \t\n]+", "", x = _) |>
        # Concatenate elements passed to dots.
        paste0(collapse = .concat) |>
        # Replace substrings of multiples space characters by a single space.
        gsub("[ \t]{2,}", " ", x = _) |>
        # Remove space characters that immediately follow new line characters.
        gsub("\n[ \t]+", "\n", x = _)

    return(str)
}

#' @rdname text
#' @keywords internal
text_hash <- function(
    .key            = "",
    .text           = "",
    .hash_algorithm = get_hash_algorithms())
{
    x <- sprintf("%s:%s", .key, .text)
    return(
        switch(.hash_algorithm,
            sha1 = digest::sha1(charToRaw(x)),
            utf8 = as.character(sum(cumsum(utf8ToInt(x)))),
            NULL))
}
