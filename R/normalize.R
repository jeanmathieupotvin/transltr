#' Normalize Text
#'
#' @description
#' Ensure consistency of input text, allowing it to be written in a variety
#' of ways.
#'
#' @details
#' In what follows, a space character is defined as being an ASCII regular
#' space or an horizontal tab (`\t`). A new line is defined an ASCII line
#' feed (`\n`).
#'
#' [normalize()] constructs a normalized string from all single-line and
#' multi-line strings passed to `...`. All underlying values are (implicitly)
#' coerced to character values in the process. It does so by going through
#' these 5 steps.
#'
#'   1. It removes implicit new lines and spaces used for indentation from
#'      multi-line strings. Empty lines are preserved.
#'   2. It replaces empty values by a new line.
#'   3. It concatenates values into a single character string using `concat`.
#'   4. It removes leading and/or trailing new lines and/or spaces, including
#'      those that could had been introduced temporarily at previous steps.
#'   5. It replaces substrings of space characters by a single space.
#'
#' @param ... Any number of character vectors containing non-[NA][base::NA]
#'   elements. They can be empty.
#'
#' @param concat A non-[NA][base::NA] character string used to concatenate
#'   values.
#'
#' @returns
#' A character string, possibly empty.
#'
#' @note
#' The author is not satisfied with the current implementation. It is *ugly*,
#' and not fast enough. Calling [gsub()] five times yields a huge performance
#' penalty. Advices are welcome. [normalize()] will be revisited in a
#' near future.
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
#' str1 <- transltr:::normalize(x1)
#' str2 <- transltr:::normalize(x2)
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
#' cat(transltr:::normalize(x), "\n")
#'
#' @keywords internal
normalize <- function(..., concat = constant("concat")) {
    assert_chr1(concat, TRUE)

    dots  <- c(...)
    empty <- which(!nzchar(dots)[-length(dots)])
    dots  <- dots |>
        gsub("\n[ \t]+", concat, x = _) |>
        gsub("^[ \t\n]+|[ \t\n]+$", "", x = _)

    dots[empty - 1L] <- paste0(dots[empty - 1L], "\n")

    return(
        dots[nzchar(dots)] |>
            paste0(collapse = concat) |>
            gsub("[ \t]*\n[ \t]*", "\n", x = _) |>
            gsub("\n+", "\n", x = _) |>
            gsub("[ \t]+", " ", x = _))
}
