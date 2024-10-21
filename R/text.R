#' Normalize Text
#'
#' @description
#' [text_normalize()] ensures consistency of input text, allowing it to be
#' written in a variety of ways.
#'
#' [text_hash()] maps an arbitrary character string (of any length) to a
#' fixed-length output.
#'
#' These functions are meant to be used as *building blocks* and does not
#' validate their inputs for maximum efficieny.
#'
#' @details
#' In what follows, a space character is defined as being an ASCII regular
#' space or an horizontal tab (`\t`). A new line is defined an ASCII line
#' feed (`\n`).
#'
#' [text_normalize()] constructs a normalized string from all single-line and
#' multi-line strings passed to `...`. All underlying values are (implicitly)
#' coerced to character values in the process. It does so by going through
#' these 5 steps.
#'
#'   1. It removes implicit new lines and spaces used for indentation from
#'      multi-line strings. Empty lines are preserved.
#'   2. It replaces empty values by a new line.
#'   3. It concatenates values into a single character string using `.concat`.
#'   4. It removes leading and/or trailing new lines and/or spaces, including
#'      those that could had been introduced temporarily at previous steps.
#'   5. It replaces substrings of space characters by a single space.
#'
#' [text_hash()] returns a reproducible hash generated from `.key` and `.text`
#' using the algorithm given by `.hash_algorithm`.
#'
#' @param ... Any number of character vectors.
#'
#' @param .concat A character string used to concatenate values.
#'
#' @param .key A character string. A language key. See [translate()].
#'
#' @param .text A character string.
#'
#' @param .hash_algorithm A character string. The algorithm to use when hashing
#'   `.key` and `.text`.
#'
#' @returns A character string. [text_hash()] returns `NULL` for unknown
#'   `.hash_algorithm` values.
#'
#' @note
#' I am not satisfied with the current implementation of [text_normalize()].
#' It *does the job*, but I believe it is (1) *ugly* and (2) not fast enough.
#' Using [gsub()] five times yields a huge performance penalty. Advices are
#' welcome. I will absolutely revisit this function in the future.
#'
#' @examples
#' x1 <- "
#'   Lorem Ipsum is simply dummy text of the printing and typesetting industry.
#'
#'   Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,
#'   when an unknown printer took a galley of type and scrambled it to make a type
#'   specimen book."
#'
#' x2 <- c(
#'   "",
#'   "Lorem Ipsum is simply dummy text of the printing and typesetting industry.",
#'   "",
#'   "Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,",
#'   "when an unknown printer took a galley of type and scrambled it to make a type",
#'   "specimen book.",
#'   "")
#'
#' str1 <- text_normalize(x1)
#' str2 <- text_normalize(x2)
#' identical(str1, str2) ## TRUE
#'
#' cat(str1, "\n")
#' cat(str2, "\n")
#'
#' ## Beware of multi-line strings missing proper indentation purposes. These
#' ## won't be normalized as expected. Use at least one space after new lines.
#' x <- "
#' Lorem Ipsum is simply dummy text of the printing and typesetting industry.
#'
#'     Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,
#'     when an unknown printer took a galley of type and scrambled it to make a type
#'     specimen book. It has survived not only five centuries, but also the leap into
#'  electronic typesetting, remaining essentially unchanged. It was popularised in
#'     the 1960s with the release of Letraset sheets containing Lorem Ipsum passages,
#'  and more recently with desktop publishing software like Aldus PageMaker
#' including versions of Lorem Ipsum."
#'
#' cat(text_normalize(x), "\n")
#'
#' @rdname text
#' @keywords internal
text_normalize <- function(..., .concat = " ") {
    dots  <- c(...)
    empty <- which(!nzchar(dots)[-length(dots)])
    dots  <- dots |>
        gsub("\n[ \t]+", .concat, x = _) |>
        gsub("^[ \t\n]+|[ \t\n]+$", "", x = _)

    dots[empty - 1L] <- paste0(dots[empty - 1L], "\n")
    return(
        dots[nzchar(dots)] |>
            paste0(collapse = .concat) |>
            gsub("[ \t]*\n[ \t]*", "\n", x = _) |>
            gsub("\n+", "\n", x = _) |>
            gsub("[ \t]+", " ", x = _))
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
            sha1 = digest::digest(
                charToRaw(x),
                algo      = "sha1",
                serialize = FALSE),
            utf8 = as.character(sum(cumsum(utf8ToInt(x)))),
            NULL))
}
