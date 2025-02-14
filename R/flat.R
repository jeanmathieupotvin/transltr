.__STR_EMPTY_LIST <- "<empty list>"

#' Serialize Objects to Flat Strings
#'
#' Serialize \R objects into textual sequences of unindented (*flat*) and
#' identifiable sections. These are called FLAT (1.0) objects.
#'
#' @details
#' The Flat format (**F**lat **L**ist **A**s **T**ext, or FLAT) is a minimal
#' textual data serialization format optimized for \R [`list`][list] objects.
#' Elements are converted to character strings and organized into unindented
#' sections identified by a tag. Call [flat_example()] for a valid example.
#'
#' [flat_serialize()] serializes `x` into a FLAT object.
#'
#' [flat_deserialize()] is the inverse operation: it converts a FLAT object
#' back into a list. The latter has the same shape as the original one, but
#'
#'   * [atomic] vectors are not reconstituted (they are deserialized as
#'     elements of length 1), and
#'   * all elements are also left as character strings.
#'
#' The convention is to serialize an empty list to an empty character string.
#'
#' ## Internal mechanisms
#'
#' [flat_tag()] and [flat_format()] are called internally by [flat_serialize()].
#' Aside from debugging purposes, they should not be called outside of the
#' former.
#'
#' [flat_tag()] creates tags from names extracted from `x` and formats them.
#' Tags may not be unique, depending on `x`'s structure and names.
#'
#' [flat_format()] recursively formats the elements of `x` as part of the
#' serialization process. It
#'
#'   * converts `NULL` to the `"NULL"` character string,
#'   * converts other elements to character strings using [format()] and
#'   * replaces empty lists by a `<empty list>` constant treated as a
#'     placeholder.
#'
#' @param x A list. It can be empty.
#'
#' @param tag_sep A non-empty and non-NA character string. The separator to use
#'   when creating tags from names (recursively) extracted from `x`.
#'
#' @param tag_empty A non-NA character string. The value to use as a substitute
#'   for empty names. Positional indices are automatically appended to it to
#'   ensure tags are always unique.
#'
#' @param string A non-NA character string. It can be empty. Contents to
#'   deserialize.
#'
#' @returns
#' [flat_serialize()] returns a character string, possibly empty.
#'
#' [flat_deserialize()] returns a named list, possibly empty. Its structure
#' depends on the underlying tags.
#'
#' [flat_tag()] returns a character vector, possibly empty.
#'
#' [flat_format()] returns an unnamed list having the same *shape* as `x`. See
#' Details.
#'
#' [flat_example()] returns a character string (a serialized example),
#' invisibly. It is used for its side-effect of printing an illustration of
#' the format (with useful information).
#'
#' @rdname flat
#' @keywords internal
flat_serialize <- function(x = list(), tag_sep = ": ", tag_empty = "") {
    # Regex in flat_deserialize() relies
    # on sep. It should not be changed.
    sep <- "\n\n"

    tags   <- flat_tag(x, tag_sep, tag_empty)
    values <- unlist(flat_format(x), TRUE, FALSE)

    return(paste0(sprintf(":: %s%s%s", tags, sep, values), collapse = sep))
}

#' @rdname flat
#' @keywords internal
flat_deserialize <- function(string = "", tag_sep = ": ") {
    assert_chr1(string, TRUE)
    assert_chr1(tag_sep)

    # Remove comments.
    # This regex matches anything that follows a single
    # octothorpe up to the next new line char or the end
    # of the string. Any preceding spaces, tabs, and new
    # lines are removed. We ignore escaped octothorpes.
    string <- gsub(r"{[ \t\n]*(?<!\\)(?:\\\\)*#(.*?)(?=\n|$)}", "", string, perl = TRUE)

    # Un-escape (regular) octothorpes (\#).
    # R automatically escape backslashes: \
    # is read as \\, so \# is read as \\#.
    string <- gsub(r"{(\\)+#}", "#", string, perl = TRUE)

    # This regex matches (and captures) titles
    # that begins by ::, ends with one or more
    # new lines, and that may begin with one or
    # more new lines (or not).
    tags_match <- gregexpr(r"{\n*\:\:[ \t]+(.*?)(?:\n+|$)}", string, perl = TRUE)[[1L]]
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
    # each tag/contents pair and sequentially define
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
        value    <- if (identical(values[[ti]], .__STR_EMPTY_LIST)) {
            list()
        } else {
            values[[ti]]
        }

        # li is the Level Index.
        for (li in lvls_ind) {
            # If a lower level is expected and the previous
            # one is not a list (a terminal node), then the
            # underlying FLAT object is invalid.
            if (li > 1L && !is.list(acc[[lvl[-li]]])) {
                stopf("'string' has an invalid structure. Check '%s' tags.",
                    paste0(lvl[-li], collapse = tag_sep))
            }

            # To recursively set values in levels of embedded
            # list objects, we have to rely on metaprogramming,
            # or on a recursive approach. Attempting to use `$`
            # (like `$`(trans, c("a", "b", ...))) does not work.
            # For each level, we check if it is defined. If it
            # is not, we create it. If it is a terminal node, a
            # value is assigned to it.
            lvl <- lvls[seq_len(li)]

            if (is.null(acc[[lvl]])) {
                acc[[lvl]] <- if (li < lvls_len) list() else value
            }
        }
    }

    return(acc)
}

#' @rdname flat
#' @keywords internal
flat_tag <- function(x = list(), tag_sep = ": ", tag_empty = "") {
    assert_list(x, TRUE)
    assert_chr1(tag_sep)
    assert_chr1(tag_empty, TRUE)

    if (!length(x)) {
        return(character())
    }

    # Preallocate an accumulator to register
    # concatenated names for each level of x.
    # It is coerced to a vector below.
    xlen <- length(x)
    acc  <- vector("list", xlen)

    # This counter is used to (incrementally)
    # ensure all tags are unique by appending
    # positions to empty/missing names.
    count <- 0L

    for (i in seq_len(xlen)) {
        # Extract names and replace them by
        # empty labels if they do not exist,
        # or if some are empty strings.
        tags <- names(x[i]) %??% rep_len(
            sprintf("%s[%s]", tag_empty, count <- count + 1L),
            length(x[i]))

        if (!all(is_nz <- nzchar(tags))) {
            # Count is offset by the current position.
            tags[!is_nz] <- sprintf("%s[%s]", tag_empty, i + count)
        }

        # Return the names if x[[i]] is not a list. Otherwise,
        # traverse it and do the same. We do not recurse into
        # special objects such as pairlists, expressions,
        # closures, etc.
        acc[[i]] <- if (is.list(x[[i]])) {
            paste0(tags, tag_sep, Recall(x[[i]], tag_sep, tag_empty))
        } else {
            tags
        }
    }

    return(unlist(acc, TRUE, FALSE))
}

#' @rdname flat
#' @keywords internal
flat_format <- function(x = list()) {
    assert_list(x, TRUE)

    if (!length(x)) {
        return(.__STR_EMPTY_LIST)
    }

    out <- lapply(x, \(el) {
        if (is.list(el)) {
            return(flat_format(el))
        }

        return(paste0(format(el), collapse = "\n"))
    })

    return(out)
}

#' @rdname flat
#' @keywords internal
flat_example <- function() {
    comments <- c(
        "# The FLAT (Flat List As Text) Format\n",
        "#\n",
        "# What follows after an octothorpe is a comment and is ignored.\n")

    obj <- list(
        Tags = list(
            Introduction    = "Tags begin by two colons and starts a new section.",
            Sections        = "This is a section. It contains a textual representation of a child element.",
            `Creating Them` = "Tags are constructed from names extracted from the original object.",
            "Missing names are substituted by a numbered standard label automatically."),
        Comments = c(
            "What follows after an octothorpe is treated as a comment and is ignored.",
            "# This is a comment.",
            "Inline comments are also allowed. # This is a comment.",
            "Escape octothorpes (\\#) to treat them as regular characters."))

    obj_flat <- flat_serialize(obj)

    cat(comments, "\n", obj_flat, "\n", sep = "")
    return(invisible(obj_flat))
}
