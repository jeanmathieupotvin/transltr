#' Source Locations
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

    lc_vec <- c(line1, col1, line2, col2)

    if (!all(lc_vec >= 1L)) {
        stops(
            "all values passed to 'line1', 'col1', 'line2', and 'col2' ",
            "must be non-NA numeric values in the range [1, Inf).")
    }
    if (!all(length(line1) == c(length(col1), length(line2), length(col2)))) {
        stops("line1', 'col1', 'line2', and 'col2' must all have the same length.")
    }

    out <- if (length(line1) > 1L) {
        # Remove duplicate ranges and sort the
        # remaining ones by their natural order.
        lc <- matrix(lc_vec,
            ncol     = 4L,
            dimnames = list(NULL, c("line1", "col1", "line2", "col2")))
        lc <- lc[order(line1, col1, line2, col2), ][!duplicated(lc), , drop = FALSE]
        c(path = path, apply(lc, 2L, identity, simplify = FALSE))
    } else {
        list(
            path  = path,
            line1 = line1,
            col1  = col1,
            line2 = line2,
            col2  = col2)
    }

    class(out) <- c("Location", "list")
    return(out)
}

#' @rdname class-location
#' @keywords internal
is_location <- function(x) {
    return(inherits(x, "Location"))
}

#' @rdname class-location
#' @export
format.Location <- function(x, how = c("long", "short"), ...) {
    assert_arg(how)
    return(
        switch(how,
            long  = format_long_location(x, ...),
            short = format_short_location(x, ...)))
}

#' @rdname class-location
#' @keywords internal
format_long_location <- function(x, ...) {
    # Align ranges by components for
    # nicer outputs when printing.
    ints   <- x[c("line1", "col1", "line2", "col2")]
    chars  <- lapply(ints, encodeString, width = NULL, justify = "right")
    ranges <- sprintf(
        "line %s, column %s @ line %s, column %s",
        chars[[1L]],
        chars[[2L]],
        chars[[3L]],
        chars[[4L]])

    x_str <- if (length(ranges) > 1L) {
        c("<Location>",
          "  Path  : " = x$path,
          "  Ranges: " = "",
          sprintf("    [%i] %s", seq_along(ranges), ranges))
    } else {
        c("<Location>",
          "  Path : " = x$path,
          "  Range: " = ranges)
    }

    return(paste0(names(x_str), x_str))
}

#' @rdname class-location
#' @keywords internal
format_short_location <- function(x, ...) {
    if (length(x$line1) > 1L) {
        stops(
            "'line1', 'col1', 'line2', and 'col2' must all have ",
            "a length equal to 1 in order to use the 'short' format.")
    }

    return(
        sprintf(
            "'%s': ln %s, col %s @ ln %s, col %s",
            x$path,
            x$line1,
            x$col1,
            x$line2,
            x$col2))
}

#' @rdname class-location
#' @export
print.Location <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    return(invisible(x))
}

#' @rdname class-location
#' @export
c.Location <- function(...) {
    if (...length() < 2L) {
        return(..1)
    }
    if (!all(vapply_1l(locs <- list(...), is_location))) {
        stops("values passed to '...' must all be 'Location' objects.")
    }

    paths <- vapply_1c(locs, `[[`, i = "path")

    if (!all(paths[[1L]] == paths[-1L])) {
        stops("all 'path' must be equal in order to combine multiple 'Location' objects.")
    }

    return(
        location(
            path  = paths[[1L]],
            line1 = unlist(lapply(locs, `[[`, i = "line1")),
            col1  = unlist(lapply(locs, `[[`, i = "col1")),
            line2 = unlist(lapply(locs, `[[`, i = "line2")),
            col2  = unlist(lapply(locs, `[[`, i = "col2"))))
}

#' @rdname class-location
#' @export
merge_locations <- function(...) {
    if (!all(vapply_1l(locs <- list(...), is_location))) {
        stops("values passed to '...' must all be 'Location' objects.")
    }

    groups <- split_ul(locs, vapply_1c(locs, `[[`, i = "path"))
    return(lapply(groups, \(group) do.call(c, group)))
}
