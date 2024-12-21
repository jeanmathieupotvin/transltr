# TODO: rewrite documentation, examples, and unit tests following new
# implementation.

#' Format Vectors
#'
#' Format atomic [vectors][vector()] and/or *recursive* structures such as
#' [lists][list()].
#'
#' The intent of [format_vector()] is to format any [vector][vector()] into a
#' set of name/value pairs (given as character strings), where indentation
#' provides information on embedded structures. The following template shows
#' how it attempts to format `x`.
#'
#' ```
#' <top_label>:
#'   <names(x[1])>: <x[1]>
#'   <names(x[2])>: <x[2]>
#'   <names(x[3])>:
#'     <names(x[[3]][1])>: <x[[3]][[1]]>
#'     ...
#'   <nokey>: <x[i]>
#'   <nokey>: <x[j]>
#'   <nokey>:
#'     <nokey>: <x[[k]][[1]]>
#'     ...
#' ```
#'
#' Names are used to construct labels. If `x[i]` has a name (`names(x[i])`
#' is not `NULL`), it is used to construct a label for `x[i]`. The format is
#' equivalent to `names(x[i]): as.character(x[i])`. If `x[i]` does not have
#' a name (or if it is empty), `<nokey>` is printed, unless `show_nokey` is
#' `FALSE`. See Examples below.
#'
#' @param x A [vector][vector()]. It cannot be empty.
#'
#' @param top_label A `NULL` or a non-empty and non-[NA][base::NA] character
#'   string used as some kind of descriptive message. Its exact meaning depends
#'   on the underlying context. In recursive calls to [format_vector()], this
#'   is set equal to the name of the current element of `x`. **It is not
#'   validated for efficiency.**
#'
#' @param indent A non-[NA][base::NA] integer value. Number of space characters
#'   to use by `levels` for indentation purposes. **It is not validated for
#'   efficiency.**
#'
#' @param show_nokey A non-[NA][base::NA] logical value. Should `NULL` and/or
#'   empty names be replaced by `"<nokey>"`? **It is not validated for
#'   efficiency.**
#'
#' @param level A non-[NA][base::NA] integer value. The current nesting level
#'   in recursive calls to [format_vector()]. **It is not validated for
#'   efficiency.**
#'
#' @returns A character vector.
#'
#' @note
#' [format_vector()] is the *workhorse* function of all S3 [format()] methods:
#' [format.Translator()], [format.Text()], and [format.Location()].
#'
#' @examples
#' x <- list(
#'   FirstName = "John",
#'   LastName  = "Doe",
#'   Address   = list(
#'     StreetAddress = "123 Main Street",
#'     City          = "Montreal",
#'     Province      = "Quebec",
#'     PostalCode    = "H0H 0H0"),
#'   Notes = c(
#'     "Send mail to",
#'     "address above."))
#'
#' cat(transltr:::format_vector(x, "<JohnDoe>"), sep = "\n")
#' cat(transltr:::format_vector(x, "<JohnDoe>", show_nokey = FALSE), sep = "\n")
#'
#' @rdname utils-format-vector
#' @family utility functions
#' @keywords internal
format_vector <- function(
    x         = vector(),
    label     = NULL,
    level     = 0L,
    indent    = 1L,
    add_names = FALSE,
    validate  = TRUE)
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
        assert_lgl1(add_names)
    }

    acc       <- vector("list", length(x) + 1L)
    acc[[1L]] <- sprintf("%s%s:",
            strrep(" ", max(0L, (level - 1L)) * indent),
            label)

    xnames <- names(x) %??% rep.int("", length(x))

    if (add_names && !all(is_nz <- nzchar(xnames))) {
        xnames[!is_nz] <- sprintf("[%i]", which(!is_nz))
    }

    for (i in seq_along(x)) {
        xi <- x[[i]]

        # NULL and empty lists are treated as
        # litteral character values to ensure
        # they are printed as expected.
        xi <- if (is.null(xi)) {
            "null"
        } else if (is.list(xi) && !length(xi)) {
            "list()"
        } else {
            xi
        }

        xi_name <- xnames[[i]]

        acc[[i + 1L]] <- if (is.list(xi) || length(xi) > 1L || is_named(xi)) {
            # Multiple values, or a single named value.
            # The latter is treated as a list to preserve the names
            # of both x[i], and x[[i]] (these are not the same thing).
            Recall(xi, xi_name, level + 1L, indent, add_names, FALSE)
        } else {
            # Single atomic values.
            sep <- if (nzchar(xi_name)) ": " else ""
            paste0(strrep(" ", level * indent), xi_name, sep, xi)
        }
    }

    return(str_trim(unlist(acc, TRUE, FALSE)))
}
