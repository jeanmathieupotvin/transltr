#' Flatten Objects
#'
#' Serialize \R objects into textual sequences of unindented (*flat*) and
#' identifiable sections.
#'
#' @details
#' The Flat format is a minimal textual data serialization format optimized
#' for [recursive][is.recursive()] objects. Elements of such objects are
#' converted to character strings and organized into unindented sections
#' identified by a tag. The resulting representation is called a *flat string*.
#'
#' [flatten()] produces flat strings which are objects of S3 class
#' [`Flat`][flatten()]. The class is relevant for printing purposes only.
#'
#' [unflatten()] is the inverse operation: it unserializes flat strings
#' back into \R objects. The latter will have the exact same *shape* as
#' the original object, but with elements coerced to character strings.
#'
#' [flatten()] is similar to a *melting process* (a *wide-to-long*
#' transformation), where each element of a [recursive][is.recursive()]
#' object is reformatted as a sequence of sections identified by a tag.
#' Tags may not be unique, depending on the input's structure and names.
#'
#' \if{html}{\out{<hr>}}
#'
#' ```
#' # Illustrating the Flat Format
#' #
#' # An octothorpe indicates that the underlying line is a comment.
#' # Comments are ignored by flat_read().
#'
#' :: Tags
#'
#' Tags always begin by two colons. They indicate the beginning of a section.
#'
#' :: Tags: Creating Them
#'
#' Tags are constructed from the names of the object being flattened.
#'
#' :: Tags: <label>[3]
#'
#' Missing names are substituted by a numbered standard label automatically.
#'
#' :: Tags: <label>[3]: <label>[1]
#'
#' The format handles any number of missing names, regardless of their depth
#' in the object being flattened.
#'
#' :: Comments
#'
#' # You may put comments anywhere, as long as they are on their own lines.
#'
#' Inline comments are disallowed. # This is not a comment.
#' ```
#'
#' \if{html}{\out{<hr>}}
#'
#' [flatten()] relies on [unlist()] to produce a character vector from a
#' [list]. However, unlike [unlist()] and [relist()], [flatten()] and
#' [unflatten()]
#'
#'   1. provide options to control how new names (**tags**) are created,
#'   2. natively allow flat strings to be reshaped back into the original
#'      object (without requiring a `skeleton`), and
#'   3. return a readily exportable textual output.
#'
#' @param x Any \R object. Its elements must be coercible to a common
#'   [atomic] type for [flatten()].
#'
#' @param tag_sep A non-empty and non-[NA][base::NA] character string. The
#'   separator to use when creating tags from names (recursively) extracted
#'   from `x`.
#'
#' @param tag_empty A non-[NA][base::NA] character string. The value to use
#'   as a substitute for empty names. Positional indices are automatically
#'   appended to it to ensure tags are always unique.
#'
#' @param string A non-[NA][base::NA] character string. A *flat* string to
#'   unserialize.
#'
#' @param path Passed as is to argument `path` of [text_read()], or
#'   [text_write()].
#'
#' @param encoding Passed as is to argument `encoding` of [text_read()], or
#'   [text_write()].
#'
#' @param keep_flat A non-[NA][base::NA] logical value. Should the flat string
#'   be kept and returned as an attribute?
#'
#' @param ... Further elements passed to, or from other methods.
#'
#' @returns
#' [flatten()] returns a character string of S3 class [`Flat`][flatten()].
#'
#' [flatten_tags()] returns a character vector.
#'
#' [unflatten()] and [flat_read()] returns a named list, possibly empty. Its
#' structure depends on the underlying tags. If `keep_flat` is `TRUE`,
#' [flat_read()] attaches a `flat` attribute to the output. The latter is a
#' character string of S3 class [`Flat`][flatten()], which is the (unparsed)
#' flat string with comments removed.
#'
#' [print()] returns argument `x` invisibly.
#'
#' [flat_write()] returns `NULL`, invisibly. It enforces `UTF-8` at all times.
#' Inputs are re-encoded if necessary.
#'
#' @note
#' The format's name can be interpreted as a recursive acronym: **F**lat
#' **L**ist **A**s **T**ext (FLAT).
#'
#' @rdname flatten
#' @keywords internal
flatten <- function(x, tag_sep = ": ", tag_empty = "") {
    assert_chr1(tag_sep)
    assert_chr1(tag_empty, TRUE)

    # NOTE: unflatten() relies on this
    # value. It should not be changed.
    sep <- "\n\n"

    tags   <- flatten_tags(x, tag_sep, tag_empty)
    values <- unlist(x, TRUE, FALSE)
    string <- paste0(sprintf(":: %s%s%s", tags, sep, values), collapse = sep)
    return(structure(string, class = "Flat"))
}

#' @rdname flatten
#' @keywords internal
flatten_tags <- function(x, tag_sep = ": ", tag_empty = "") {
    # Preallocate an accumulator to register
    # concatenated names for each level of x.
    # It is coerced to a vector below.
    xlen  <- length(x)
    acc   <- vector("list", xlen)
    count <- 0L

    for (i in seq_len(xlen)) {
        # Extract names, and replace them by
        # empty labels if they do not exist,
        # or if some are empty strings.
        tags <- names(x[i]) %??% rep_len(
            sprintf("%s[%s]", tag_empty, count <- count + 1L),
            length(x[i]))

        if (!all(is_nz <- nzchar(tags))) {
            tags[!is_nz] <- sprintf("%s[%s]", tag_empty, i + count)
        }

        # Return the names if x[[i]] is not
        # recursive (not a list-like object).
        # Otherwise, traverse it, and do the
        # same thing.
        acc[[i]] <- if (is.recursive(x[[i]])) {
            paste0(tags, tag_sep, Recall(x[[i]], tag_sep, tag_empty))
        } else {
            tags
        }
    }

    return(unlist(acc, TRUE, FALSE))
}

#' @rdname flatten
#' @keywords internal
unflatten <- function(string, tag_sep = ": ") {
    assert_chr1(string, TRUE)
    assert_chr1(tag_sep)

    if (!nzchar(string)) {
        return(list())
    }

    # The following regex matche (and captures)
    # titles that begins by ::, ends with one or
    # more new lines, and that may begin with one
    # or more new lines (or not).
    tags_match <- gregexpr(r"{\n*\:\:[ \t]+(.*?)\n+}", string, perl = TRUE)[[1L]]
    tags_first <- attr(tags_match, "capture.start")[, 1L]
    tags_last  <- tags_first + attr(tags_match, "capture.length")[, 1L] - 1L
    tags       <- substring(string, tags_first, tags_last)
    tags_lvls  <- strsplit(tags, tag_sep, TRUE)

    # Contents begins immediately after the end
    # of its underlying tag. Any empty/new line
    # char is ignored because it is included in
    # matches above (see tags_match). In other
    # words, leading and trailing new lines are
    # not considered to be part of the contents.
    values <- substring(string,
        # Contents starts after its tag (and its
        # trailing new lines).
        first = tags_match + attr(tags_match, "match.length"),
        # Contents ends just before the next tag starts,
        # or at the end of the string for the last one.
        last = c(tags_match[-1L] - 1L, nchar(string)))

    # Recursively restructuring list-like objects is
    # harder to achieve with usual R patterns. Below,
    # we start with an empty accumulator, loops over
    # each tag/contents pair, and sequentially define
    # levels within it. Since we grow an object, this
    # is usually a no go, but this is actually very
    # fast (parsing is completed in microseconds).
    acc <- list()

    # ti is the Tag Index.
    # Each tag has one or more levels. A level
    # is a constituent extracted from a tag.
    for (ti in seq_along(tags_lvls)) {
        lvls     <- tags_lvls[[ti]]
        lvls_len <- length(lvls)
        lvls_ind <- seq_along(lvls)

        # li is the Level Index.
        for (li in lvls_ind) {
            # To recursively set values in levels of embedded
            # list objects, we have to rely on metaprogramming,
            # or on a recursive approach. Attempting to use `$`
            # (like `$`(trans, c("a", "b", ...))) does not work.
            # For each level, we check if it is defined. If it
            # is not, we create it. If it is a terminal node, a
            # value is assigned to it.
            lvl <- lvls[seq_len(li)]

            if (is.null(acc[[lvl]])) {
                acc[[lvl]] <- if (li < lvls_len) list() else values[[ti]]
            }
        }
    }

    return(acc)
}

#' @rdname flatten
#' @export
print.Flat <- function(x, ...) {
    cat("<Flat>", str_wrap(x, width = 80L), sep = "\n")
    return(invisible(x))
}

#' @rdname flatten
#' @keywords internal
flat_read <- function(
    path      = "",
    encoding  = "UTF-8",
    tag_sep   = ": ",
    keep_flat = FALSE)
{
    assert_lgl1(keep_flat)

    text   <- text_read(path, encoding)
    string <- paste0(text[!startsWith(text, "#")], collapse = "\n")
    out    <- unflatten(string, tag_sep)

    if (keep_flat) {
        attr(out, "flat") <- structure(string, class = "Flat")
    }

    return(out)
}

#' @rdname flatten
#' @keywords internal
flat_write <- function(x, path = "", tag_sep = ": ", tag_empty = "") {
    string <- flatten(x, tag_sep, tag_empty)
    return(text_write(string, path))
}
