#' Format Vectors
#'
#' @description
#' Format [atomic][vector()] vectors, [lists][list()], and
#' [pairlists][pairlist()].
#'
#' @details
#' [format_vector()] is an alternative to [utils::str()] that exposes a much
#' simpler generic formatting interface and yields terser outputs of name/value
#' pairs. Indentation is used for nested values.
#'
#' [format_vector()] does not attempt to cover all \R objects like
#' [utils::str()]. Instead, it (merely) focuses on efficiently handling the
#' types used by [`transltr`]. It is the low-level *workhorse* function of
#' [format.Translator()], [format.Text()], and [format.Location()].
#'
#' @param x A [vector][vector()] of any [atomic][vector()] mode, a
#'   [list][list()], or a [pairlist][pairlist()]. It can be empty
#'   and it can contain NA values.
#'
#' @param label A `NULL`, or a non-empty and non-NA character string. A top
#'   descriptive label for `x`. It is used to preserve, and output all names
#'   in recursive calls. The value passed to `label` is considered to be at
#'   `level` 0, and is not indented.
#'
#' @param level A non-NA integer value. The current depth, or current
#'   *nesting level* to use for indentation purposes.
#'
#' @param indent A non-NA integer value. The number of single space(s) to use
#'   for each `level` when indenting name/value pairs.
#'
#' @param fill_names A non-NA logical value. Should `NULL` and empty names
#'   be replaced by names created from the elements' underlying positions?
#'   Positions are relative to each `level`.
#'
#' @param null A non-empty and non-NA character string. The value to use to
#'   represent `NULL` and empty parlists (they are conceptually the same thing).
#'
#' @param empty A non-empty and non-NA character string. The value to use to
#'   represent empty vectors, excluding `NULL`. See `null` above for the latter.
#'   The [type][typeof()] of the underlying empty object is added to `empty`
#'   for convenience. See Examples below.
#'
#' @param validate A non-NA logical value. Should the arguments be validated
#'   before being used? This argument should be left as is.
#'
#' @returns
#' A character vector, possibly trimmed by [str_trim()].
#'
#' @seealso
#' [str_trim()]
#'
#' @rdname utils-format-vector
#' @family utility functions
#' @keywords internal
format_vector <- function(
    x          = vector(),
    label      = NULL,
    level      = 0L,
    indent     = 1L,
    fill_names = FALSE,
    null       = "<null>",
    empty      = "<empty>",
    validate   = TRUE)
{
    assert_lgl1(validate)

    if (validate) {
        if (!is.null(label) && !is_chr1(label)) {
            stops("'label' must be a non-NA and non-empty character of length 1, or 'NULL'.")
        }

        assert_int1(level)
        assert_between(level, 0L)
        assert_int1(indent)
        assert_between(indent, 0L)
        assert_lgl1(fill_names)
        assert_chr1(null)
        assert_chr1(empty)
    }

    # Define an accumulator of formatted values.
    # It is coerced to an atomic vector (likely
    # a character) below.
    acc <- vector("list", length(x) + 1L)

    # Set (top) label. Values that yield either
    # NULL, or character(0) are discarded by
    # unlist() below.
    label_indent <- strrep(" ", max(0L, (level - 1L)) * indent)
    acc[[1L]]    <- sprintf("%s%s:", label_indent, label)

    xnames <- names(x) %??% rep.int("", length(x))

    # Missing names are replaced by names created
    # from the elements' underlying indices.
    if (fill_names && !all(is_nz <- nzchar(xnames))) {
        xnames[!is_nz] <- sprintf("[%i]", which(!is_nz))
    }

    # Loop over each element of x and
    # accumulate formatted values in acc.
    for (i in seq_along(x)) {
        i_name <- xnames[[i]]
        i_x    <- x[[i]]

        # Print element in a (more) semantic way.
        i_x <- if (is.null(i_x)) {
            # Print NULL (and empty pairlists)
            # as a literal character string.
            null
        } else if (!length(i_x)) {
            # Print other empty objects as literal
            # character strings that state their
            # emptiness and their type.
            sprintf("%s [%s]", empty, typeof(i_x))
        } else if (is.character(i_x) && !all(is_nz <- nzchar(i_x))) {
            # Empty character strings are printed in a
            # way¸that clearly conveys their emptiness.
            i_x[!is_nz] <- r"{""}"
            i_x
        } else {
            i_x
        }

        acc[[i + 1L]] <- if (is.recursive(i_x) || length(i_x) > 1L || is_named(i_x)) {
            # Multiple values, or any named value embedded in a lower level.
            Recall(
                i_x,
                label      = i_name,
                level      = level + 1L,
                indent     = indent,
                fill_names = fill_names,
                null       = null,
                empty      = empty,
                validate   = FALSE)
        } else if (nzchar(i_name)) {
            # Single named atomic value.
            paste0(strrep(" ", level * indent), i_name, ": ", i_x)
        } else {
            # Single unnamed atomic value.
            paste0(strrep(" ", level * indent), i_x)
        }
    }

    # Outputs is trimmed to a maximum of 77 chars
    # (3 are reserved for a '...' suffix, see doc).
    return(str_trim(unlist(acc, TRUE, FALSE)))
}
