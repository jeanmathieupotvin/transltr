#' Source Locations
#'
#' Structure and manipulate source locations. Class
#' [`SourceLocation`][SourceLocation] is a lighter alternative to [srcfile()]
#' and other related functionalities.
#'
#' A [`SourceLocation`][SourceLocation] is a set of one or more line/column
#' ranges referencing textual contents within a generic *container*: a file
#' on disk, on a network, a binding, a location in memory, a buffer, etc.
#' This origin is identified by an underlying `path`.
#'
#' [`SourceLocation`][SourceLocation] objects may refer to multiple locations
#' within the same origin. This is why arguments `line1`, `col1`, `line2` and
#' `col2` accept integer vectors of length greater than 1.
#'
#' ## Combining `SourceLocation` Objects
#'
#' [c()] can only combine [`SourceLocation`][SourceLocation] objects having the
#' same `path`. The underlying ranges are combined into a set of non-duplicated
#' range(s).
#'
#' [merge_source_locations()] is a generalized version of [c()] that handles
#' any number of [`SourceLocation`][SourceLocation] objects (possibly having
#' different paths).
#'
#' @param path A non-empty and non-NA character string. The origin of the
#'   range(s) delimited by `line1`, `col1`, `line2`, `col2`.
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
#'   * Any number of [`SourceLocation`][SourceLocation] objects for
#'     [merge_source_locations()] and S3 method [c()].
#'   * Further arguments passed to or from other methods for [format()] and
#'     [print()].
#'
#' @returns
#' [source_location()], and [c()] return a named list of length 5 and of S3
#' class [`SourceLocation`][SourceLocation] containing the values of `path`,
#' `line1`, `col1`, `line2`, and `col2`.
#'
#' [is_source_location()] returns a logical value.
#'
#' [format()] returns a character vector.
#'
#' [print()] returns argument `x` invisibly.
#'
#' [merge_source_locations()] returns a list of [`SourceLocation`][SourceLocation]
#' objects.
#'
#' @examples
#' # Create SourceLocation objects.
#' loc1 <- source_location("file-a", 1L, 2L, 3L, 4L)
#' loc2 <- source_location("file-a", 5L, 6L, 7L, 8L)
#' loc3 <- source_location("file-c", c(9L, 10L), c(11L, 12L), c(13L, 14L), c(15L, 16L))
#'
#' is_source_location(loc1)  ## TRUE
#'
#' print(loc1)
#' print(loc2)
#' print(loc3)
#'
#' # Combine SourceLocation objects.
#' # c() throws an error if they do not have the same path.
#' c(loc1, loc2)
#'
#' # SourceLocation objects with different paths can be merged.
#' # This groups SourceLocation objects according to their paths
#' # and calls c() on each group. It returns a list.
#' merge_source_locations(loc1, loc2, loc3)
#'
#' # The path of a SourceLocation object can be whatever the user wants.
#' # Below is an example that references text in a character bound to
#' # variable x in the global environment.
#' x <- "This is a string and it is held in memory."
#' source_location("<env: R_GlobalEnv: x>", 1L, 11L, 1L, 16L) ## "string"
#'
#' @aliases SourceLocation
#' @rdname class-source-location
#' @keywords internal
#' @export
source_location <- function(
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
        lc <- matrix(lc_vec, ncol = 4L)
        lc <- lc[order(line1, col1, line2, col2), ][!duplicated(lc), , drop = FALSE]

        line1 <- lc[, 1L]
        col1  <- lc[, 2L]
        line2 <- lc[, 3L]
        col2  <- lc[, 4L]
    }

    return(
        structure(
            list(
                id    = hash_str(path),
                path  = path,
                line1 = line1,
                col1  = col1,
                line2 = line2,
                col2  = col2),
            class = c("SourceLocation", "list")))
}

#' @rdname class-source-location
#' @keywords internal
#' @export
is_source_location <- function(x) {
    return(inherits(x, "SourceLocation"))
}

#' @rdname class-source-location
#' @export
format.SourceLocation <- function(x, indent = 1L, ...) {
    return(format_vector(as.list(x, ...), level = indent, indent = 1L))
}

#' @rdname class-source-location
#' @export
print.SourceLocation <- function(x, ...) {
    cat("<SourceLocation>", format(x, ...), sep = "\n")
    return(invisible(x))
}

#' @rdname class-source-location
#' @export
as.list.SourceLocation <- function(x, parse_range = TRUE, ...) {
    assert_lgl1(parse_range)

    if (parse_range) {
        return(list(id = x$id, path = x$path, ranges = range_format(x)))
    }

    return(unclass(x))
}

#' @rdname class-source-location
#' @export
c.SourceLocation <- function(...) {
    if (...length() < 2L) {
        return(..1)
    }
    if (!all(vapply_1l(locs <- list(...), is_source_location))) {
        stops("values passed to '...' must all be 'SourceLocation' objects.")
    }

    ids <- vapply_1c(locs, `[[`, i = "id")

    if (!all(ids[[1L]] == ids[-1L])) {
        stops("all '$path' (and '$id') must be equal to combine 'SourceLocation' objects.")
    }

    return(
        source_location(
            path  = ..1$path,
            line1 = unlist(lapply(locs, `[[`, i = "line1")),
            col1  = unlist(lapply(locs, `[[`, i = "col1")),
            line2 = unlist(lapply(locs, `[[`, i = "line2")),
            col2  = unlist(lapply(locs, `[[`, i = "col2"))))
}

#' @rdname class-source-location
#' @keywords internal
#' @export
merge_source_locations <- function(...) {
    if (!all(vapply_1l(locs <- list(...), is_source_location))) {
        stops("values passed to '...' must all be 'SourceLocation' objects.")
    }

    groups <- unname(split(locs, vapply_1c(locs, `[[`, i = "id")))
    merged <- lapply(groups, \(group) do.call(c, group))
    names(merged) <- vapply_1c(merged, `[[`, i = "id")
    return(merged)
}

#' Source Ranges
#'
#' Create, parse, and validate source ranges.
#'
#' Ranges are `r range_std_template("user")` strings created on-the-fly from
#' [`SourceLocation`][SourceLocation] objects for outputting purposes.
#'
#' @param x A [`SourceLocation`][SourceLocation] object.
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
#' [range_std_template()] returns a constant character string. This is a
#' human-readable template showing how source ranges must be formatted.
#'
#' [range_std_template()] returns a constant character string. This is a regular
#' expression used to detect and parse candidate source ranges.
#'
#' @seealso
#' [`SourceLocation`][SourceLocation],
#' [`ExportedSourceLocation`][ExportedSourceLocation],
#'
#' @rdname class-source-location-ranges
#' @keywords internal
range_format <- function(x = source_location()) {
    if (!is_source_location(x)) {
        stops("'x' must be a 'SourceLocation' object.")
    }

    bounds <- x[c("line1", "col1", "line2", "col2")]
    chars <- lapply(bounds, encodeString, width = NULL, justify = "right")

    return(
        sprintf(
            range_std_template("sprintf"),
            bounds$line1,
            bounds$col1,
            bounds$line2,
            bounds$col2))
}

#' @rdname class-source-location-ranges
#' @keywords internal
range_parse <- function(ranges = character()) {
    assert_chr(ranges)

    matches <- gregexpr(range_std_template("regex"), ranges, perl = TRUE)
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

#' @rdname class-source-location-ranges
#' @keywords internal
range_is_parseable <- function(ranges = character()) {
    assert_chr(ranges)
    return(grepl(range_std_template("regex"), ranges))
}

#' @rdname class-source-location-ranges
#' @keywords internal
range_std_template <- function(style = c("user", "sprintf", "regex")) {
    assert_chr1(style)
    assert_arg(style, TRUE)
    return(
        switch(style,
            sprintf = "Ln %s, Col %s @ Ln %s, Col %s",
            user    = "`Ln <int>, Col <int> @ Ln <int>, Col <int>`",
            regex   = sprintf(
                "^%s%s%1$s$",
                # Regex to detect 'Ln X, Col Y'.
                # Space(s) and tab(s) are allowed.
                "Ln[ \t]*([0-9.]+),[ \t]*Col[ \t]*([0-9.]+)",
                # Regex to detect ' @ '.
                # Space(s) and tab(s) are allowed.
                "[ \t]*@[ \t]*")))
}
