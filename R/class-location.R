#' Source Locations
#'
#' Store, structure, and manipulate source locations. Class
#' [`Location`][Location] is a lighter alternative to [srcfile()] and
#' related functionalities.
#'
#' A [`Location`][Location] is a set of one or more line/column ranges
#' referencing contents (like text or source code) within a common *origin*
#' identified by an underlying `path`. The latter is generic and can be
#' *anything*: a file on disk, on a network, a pointer, a binding, etc. What
#' matters is the underlying context.
#'
#' [`Location`][Location] objects may refer to multiple distinct ranges for
#' the the same origin. This is why arguments `line1`, `col1`, `line2`, and
#' `col2` accept integer vectors (and not only scalar values).
#'
#' ## Combining Location Objects
#'
#' [c()] can only combine [`Location`][Location] objects having the same
#' `path`. In that case, the underlying ranges are combined into a set of
#' non-duplicated range(s).
#'
#' [merge_locations()] is a generalized version of [c()] that handles any
#' number of [`Location`][Location] objects having possibly different paths.
#' It can be viewed as a vectorized version of [c()].
#'
#' @param path A non-empty and non-[NA][base::NA] character string. The origin
#'   of the range(s).
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
#' @param how A character string equal to `"long"` or `"short"`. The latter is
#'   suitable for embedding [`Location`][Location] objects in
#'   [messages][message()], and [conditions].
#'
#' @param ... Usage depends on the underlying function.
#'   * Any number of [`Location`][Location] objects for [merge_locations()]
#'     and S3 method [c()].
#'   * Further arguments passed to or from other methods for [format()] and
#'     [print()].
#'
#' @returns
#' [location()] and [c()] returns a named list of length 5 and of class
#' [`Location`][Location]. It contains the values of `path`, `line1`, `col1`,
#' `line2`, and `col2`.
#'
#' [is_location()] returns a logical value.
#'
#' [format()] returns a character vector. If `how` is equal to `"short"`, it
#' is of length 1.
#'
#' [print()] returns argument `x` invisibly.
#'
#' [merge_locations()] returns a list of (combined) [`Location`][Location]
#' objects.
#'
#' @examples
#' ## Create Location objects.
#' loc1 <- location("a", 1L, 2L, 3L, 4L)
#' loc2 <- location("a", 5L, 6L, 7L, 8L)
#' loc3 <- location("c", c(9L, 10L), c(11L, 12L), c(13L, 14L), c(15L, 16L))
#'
#' ## Combine Location objects.
#' c(loc1, loc2)
#' merge_locations(loc1, loc2, loc3)
#'
#' ## Using a Location object to reference text in an R
#' ## character vector stored in a named environment.
#' x <- "This is a string and it is held in memory for some purpose."
#' location("<environment: R_GlobalEnv: x>", 1L, 11L, 1L, 16L)  ## "string"
#'
#' @aliases Location
#' @rdname class-location
#' @keywords internal
#' @export
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
#' @export
is_location <- function(x) {
    return(inherits(x, "Location"))
}

#' @rdname class-location
#' @export
format.Location <- function(x, how = c("long", "short"), ...) {
    assert_arg(how)
    return(
        switch(how,
            long  = .format_long_location(x, ...),
            short = .format_short_location(x, ...)))
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
#' @export
merge_locations <- function(...) {
    if (!all(vapply_1l(locs <- list(...), is_location))) {
        stops("values passed to '...' must all be 'Location' objects.")
    }

    groups <- split_ul(locs, vapply_1c(locs, `[[`, i = "path"))
    return(lapply(groups, \(group) do.call(c, group)))
}


# Internal functions -----------------------------------------------------------


.format_long_location <- function(x, ...) {
    chars <- lapply(x, encodeString, width = NULL, justify = "right")
    xlist <- list(
        Path   = x$path,
        Ranges = sprintf(
            "line %s, column %s @ line %s, column %s",
            chars[[2L]],
            chars[[3L]],
            chars[[4L]],
            chars[[5L]]))

    return(format_vector(xlist, "<Location>", .show_nokey = FALSE))
}

.format_short_location <- function(x, ...) {
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
