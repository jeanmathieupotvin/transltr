#' Source locations
#'
#' An internal class that represents source locations. A *source location* is a
#' a range of source lines of a given source script. As such, it may reference
#' anything.
#'
#' @param path A non-empty and non-[NA][base::NA] character string. The
#'   underlying source file.
#'
#' @param line1,col1 A non-[NA][base::NA] integer. The (inclusive) starting
#'   point of what is being referenced.
#'
#' @param line2,col2 A non-[NA][base::NA] integer. The (inclusive) end of what
#'   is being referenced.
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
    assert_int1(line1)
    assert_int1(col1)
    assert_int1(line2)
    assert_int1(col2)
    assert_between(line1, 1L)
    assert_between(col1,  1L)
    assert_between(line2, 1L)
    assert_between(col2,  1L)

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
    return(do.call(sprintf, c(fmt = "%s: ln %i, col %i @ ln %i, col %i", x)))
}

#' @rdname class-location
#' @export
print.Location <- function(x, ...) {
    cat("<Location> ", format(x, ...), "\n", sep = "")
    return(invisible(x))
}
