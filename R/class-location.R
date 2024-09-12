#' Source locations
#'
#' An internal class to represent source locations.
#'
#' A *source location* is a line/column range within any text file.
#' [`Location`][Location] objects may refer multiple distinct ranges within
#' the same source script. This is why arguments `line1`, `col1`, `line2`, and
#' `col2` accept integer vectors (and not only scalar values).
#'
#' @param path A non-empty and non-[NA][base::NA] character string. The
#'   underlying source file.
#'
#' @param line1,col1 A non-empty integer vector of non-[NA][base::NA] values.
#'   The (inclusive) starting point(s) of what is being referenced.
#'
#' @param line2,col2 A non-empty integer vector of non-[NA][base::NA] values.
#'   The (inclusive) end(s) of what is being referenced.
#'
#' @param x Any \R object. Obviously, an object of class [`Location`][Location]
#'   for the S3 methods defined here.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @returns
#' [location()] returns a named list of length 5 and of class
#' [`Location`][Location]. It contains the values of `path`, `line1`, `col1`,
#' `line2`, and `col2`.
#'
#' [is_location()] returns a logical.
#'
#' [format.Location()] returns a character string.
#'
#' [print.Location()] returns argument `x` invisibly.
#'
#' @note
#' [location()] is mostly used to restructure information returned by
#' [utils::getParseData()].
#'
#' @aliases Location
#' @rdname class-location
#' @keywords internal
location <- function(
    path  = tempfile(),
    line1 = 1L,
    col1  = 1L,
    line2 = 1L,
    col2  = 1L)
{
    assert_chr1(path)
    assert_int(line1)
    assert_int(col1)
    assert_int(line2)
    assert_int(col2)

    if (!all(vapply_1l(c(line1, line2, col1, col2), is_between, min = 1L))) {
        stops(
            "all values passed to 'line1', 'col1', 'line2', and 'col2' ",
            "must be non-NA numeric values in the range [1, Inf).")
    }
    if (!all(length(line1) == c(length(col1), length(line2), length(col2)))) {
        stops("line1', 'col1', 'line2', and 'col2' must all have the same length.")
    }

    return(
        structure(
            list(
                path  = path,
                line1 = line1,
                col1  = col1,
                line2 = line2,
                col2  = col2),
            class = c("Location", "list")))
}

#' @rdname class-location
#' @keywords internal
is_location <- function(x) {
    return(inherits(x, "Location"))
}

#' @rdname class-location
#' @export
format.Location <- function(x, ...) {
    # Format of elements in a printf style.
    # We use a base padding of 2 spaces.
    fmt_path   <- "  '%s':"
    fmt_ranges <- "    - line %s, column %s @ line %s, column %s"

    # Align ranges by components for
    # nice outputs when printing.
    integers <- x[c("line1", "col1", "line2", "col2")]
    chars    <- lapply(integers, encodeString, width = NULL, justify = "right")

    # Integers are converted to strings when
    # padded below. We use %s instead of %i.
    return(c(
        "<Location>",
        sprintf(fmt_path, x$path),
        do.call(sprintf, c(fmt = fmt_ranges, chars))))
}

#' @rdname class-location
#' @export
print.Location <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    return(invisible(x))
}
