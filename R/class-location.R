.__STR_RANGE_USR_FMT <- "`Ln <int>, Col <int> @ Ln <int>, Col <int>`"
.__STR_RANGE_REGEX   <- paste(
    "^Ln[ \t]*([0-9.]+),[ \t]*Col[ \t]*([0-9.]+)",
    "[ \t]*@[ \t]*",
    "Ln[ \t]*([0-9.]+),[ \t]*Col[ \t]*([0-9.]+)$")

#' Source Locations
#'
#' Structure and manipulate source locations. Class [`Location`][Location] is
#' a lighter alternative to [srcfile()] and other related functionalities.
#'
#' A [`Location`][Location] is a set of one or more line/column ranges
#' referencing contents (like text or source code) within a common *origin*
#' identified by an underlying `path`. The latter is generic and can be
#' *anything*: a file on disk, on a network, a pointer, a binding, etc. What
#' matters is the underlying context.
#'
#' [`Location`][Location] objects may refer to multiple distinct ranges for
#' the the same origin. This is why arguments `line1`, `col1`, `line2` and
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
#' @param path A non-empty and non-NA character string. The origin of the ranges.
#'
#' @param line1,col1 A non-empty integer vector of non-NA values. The
#'   (inclusive) starting point(s) of what is being referenced.
#'
#' @param line2,col2 A non-empty integer vector of non-NA values. The
#'   (inclusive) end(s) of what is being referenced.
#'
#' @param x Any \R object.
#'
#' @param ... Usage depends on the underlying function.
#'   * Any number of [`Location`][Location] objects for [merge_locations()]
#'     and S3 method [c()].
#'   * Further arguments passed to or from other methods for [format()] and
#'     [print()].
#'
#' @returns
#' [location()], and [c()] return a named list of length 5 and of S3 class
#' [`Location`][Location] containing the values of `path`, `line1`, `col1`,
#' `line2`, and `col2`.
#'
#' [is_location()] returns a logical value.
#'
#' [format()] returns a character vector.
#'
#' [print()] returns argument `x` invisibly.
#'
#' [merge_locations()] returns a list of (combined) [`Location`][Location]
#' objects.
#'
#' @examples
#' # Create Location objects.
#' loc1 <- location("file-a", 1L, 2L, 3L, 4L)
#' loc2 <- location("file-a", 5L, 6L, 7L, 8L)
#' loc3 <- location("file-c", c(9L, 10L), c(11L, 12L), c(13L, 14L), c(15L, 16L))
#'
#' is_location(loc1)  ## TRUE
#'
#' print(loc1)
#' print(loc2)
#' print(loc3)
#'
#' # Combine Location objects.
#' # c() throws an error if they do not have the same path.
#' c(loc1, loc2)
#'
#' # Location objects with different paths can be merged.
#' # This groups Location objects according to their paths
#' # and calls c() on each group. It returns a list.
#' merge_locations(loc1, loc2, loc3)
#'
#' # The path of a Location object can be whatever fits the context.
#' # Below is an example that references text in a character vector
#' # bound to variable x in the global environment.
#' x <- "This is a string and it is held in memory for some purpose."
#' location("<environment: R_GlobalEnv: x>", 1L, 11L, 1L, 16L)
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
format.Location <- function(x, ...) {
    xlist <- list(Path = x$path, Ranges = range_format(x))
    return(c("<Location>", format_vector(xlist, level = 1L)))
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
        stops("all 'path' must be equal in order to combine 'Location' objects.")
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

    groups <- unname(split(locs, vapply_1c(locs, `[[`, i = "path")))
    return(lapply(groups, \(group) do.call(c, group)))
}

#' Source Ranges
#'
#' Create, parse, and validate source ranges.
#'
#' Ranges are `r .__STR_RANGE_USR_FMT` strings created on-the-fly from
#' [`Location`][Location] objects for outputting purposes.
#'
#' @param x A [`Location`][Location] object.
#'
#' @param ranges A character vector of non-NA and non-empty values.
#'   The ranges to extract pairs of indices (line, column) from.
#'
#' @returns
#' [range_format()] returns a character vector. It assumes that `x` is valid.
#'
#' [range_parse()] returns a list having the same length as `ranges`. Each
#' element is an integer vectors containing 4 non-NA values (unless the
#' underlying range is invalid).
#'
#' [range_is_parseable()] returns a logical vector having the same length as
#' `ranges`.
#'
#' @seealso
#' [`Location`][Location],
#' [`ExportedLocation`][ExportedLocation],
#'
#' @rdname class-location-ranges
#' @keywords internal
range_format <- function(x = location()) {
    if (!is_location(x)) {
        stops("'x' must be a 'Location' object.")
    }

    chars <- lapply(x[-1L], encodeString, width = NULL, justify = "right")
    return(do.call(sprintf, c("Ln %s, Col %s @ Ln %s, Col %s", chars)))
}

#' @rdname class-location-ranges
#' @keywords internal
range_parse <- function(ranges = character()) {
    assert_chr(ranges)

    matches <- gregexpr(.__STR_RANGE_REGEX, ranges, perl = TRUE)
    starts  <- sapply(matches, attr, "capture.start")
    widths  <- sapply(matches, attr, "capture.length")
    ends    <- starts + widths - 1L

    # as.integer() may throw warnings
    # if characters cannot be coerced.
    # We hide these from the user, as
    # there is nothing else they can do.
    numbers <- suppressWarnings(
        lapply(seq_along(ranges), \(i)  {
            as.integer(substring(ranges[[i]], starts[, i], ends[, i]))
        })
    )

    return(numbers)
}

#' @rdname class-location-ranges
#' @keywords internal
range_is_parseable <- function(ranges = character()) {
    assert_chr(ranges)
    return(grepl(.__STR_RANGE_REGEX, ranges))
}
