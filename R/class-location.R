#' Source Locations
#'
#' Structure and manipulate source locations. Class [`Location`][Location]
#' is a more convenient alternative to [srcfile()].
#'
#' A *source location* is defined as a set of one or more ranges referencing
#' contents wihin a common *origin* that can be located via its underlying
#' `path`. The latter can be anything: a file on disk, on a network, an object
#' in memory, etc.
#'
#' [`Location`][Location] objects may refer multiple distinct ranges within
#' the same source script. This is why arguments `line1`, `col1`, `line2`, and
#' `col2` accept integer vectors (and not only scalar values).
#'
#' @param path A non-empty and non-[NA][base::NA] character string identifying
#'   the origin of the range(s).
#'
#' @param line1,col1 A non-empty integer vector of non-[NA][base::NA] values.
#'   The (inclusive) starting point(s) of what is being referenced.
#'
#' @param line2,col2 A non-empty integer vector of non-[NA][base::NA] values.
#'   The (inclusive) end(s) of what is being referenced.
#'
#' @param x Any \R object for [is_location()]. An object of class
#'   [`Location`][Location] for S3 methods [format()] and [print()].
#'
#' @param ... Usage depends on the underlying function.
#'   * Further arguments passed to or from other methods for [format()] and
#'     [print()].
#'   * Any number of [`Location`][Location] objects for [merge_locations()]
#'     and S3 method [c()].
#'
#' @returns
#' [location()] returns a named list of length 5 and of class
#' [`Location`][Location]. It contains the values of `path`, `line1`, `col1`,
#' `line2`, and `col2`.
#'
#' [is_location()] returns a logical.
#'
#' [format()] returns a character. If `how` is equal to `"short"`, it is of
#' length 1.
#'
#' [print()] returns argument `x` invisibly.
#'
#' [c()] returns a [`Location`][Location] object. It can only combine objects
#' having the exact same `path`. In that case, ranges are combined into a
#' coherent set of unique range(s).
#'
#' [merge_locations()] returns a list of [`Location`][Location] objects. It is
#' a generalized version of [c()] that handles [`Location`][Location] objects
#' having different path(s).
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

    if (length(line1) > 1L) {
        # Remove duplicated ranges and sort the
        # remaining ones by their natural order.
        lc    <- matrix(lc_vec, ncol = 4L)
        lc    <- lc[order(line1, col1, line2, col2), ][!duplicated(lc), , drop = FALSE]
        line1 <- lc[, 1L]
        col1  <- lc[, 2L]
        line2 <- lc[, 3L]
        col2  <- lc[, 4L]
    }

    out <- list(
        path  = path,
        line1 = line1,
        col1  = col1,
        line2 = line2,
        col2  = col2)

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
#' @keywords internal
merge_locations <- function(...) {
    if (!all(vapply_1l(locs <- list(...), is_location))) {
        stops("values passed to '...' must all be 'Location' objects.")
    }

    groups <- split_ul(locs, vapply_1c(locs, `[[`, i = "path"))
    return(lapply(groups, \(group) do.call(c, group)))
}

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
          "  Ranges:"  = "",
          sprintf("    [%i] %s", seq_along(ranges), ranges))
    } else {
        c("<Location>",
          "  Path : " = x$path,
          "  Range: " = ranges)
    }

    return(paste0(names(x_str), x_str))
}

format_short_location <- function(x, ...) {
    if (length(x$line1) > 1L) {
        stops(
            "'line1', 'col1', 'line2', and 'col2' must all have ",
            "a length equal to 1 in order to use the 'short' format.")
    }

    return(
        sprintf(
            "%s: ln %s, col %s @ ln %s, col %s",
            x$path,
            x$line1,
            x$col1,
            x$line2,
            x$col2))
}
