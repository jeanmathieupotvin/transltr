#' Internal Constants
#'
#' Fetch shared constants used by [`transltr`][transltr]. Constants ensure
#' consistency among all features of the package.
#'
#' @returns
#' A character string, or `NULL` if value passed to `which` is unavailable.
#'
#' @examples
#' constant("concat")        ## Outputs " "
#' constant("empty")         ## Outputs "<none>"
#' constant("unset")         ## Outputs "<unset>"
#' constant("unknown")       ## Outputs "<unknown>"
#' constant("untranslated")  ## Outputs "# Erase this comment and provide a translation."
#'
#' # NULL is returned if which has no corresponding entry.
#' constant("__undefined__")
#'
#' @rdname constants
#' @keywords internal
#' @export
constant <- function(which = c(
    "concat",
    "empty",
    "unset",
    "unknown",
    "untranslated"))
{
    assert_chr1(which)
    return(
        switch(which,
            concat       = " ",
            empty        = "<none>",
            unset        = "<unset>",
            unknown      = "<unknown>",
            untranslated = "# Erase this comment and provide a translation.",
            NULL))
}

#' @format
#' `.__LGL_DEBUG_FLAG` is an internal logical value always equal to `FALSE`
#' used to force errors that are otherwise hard, or (almost) impossible to
#' test otherwise. It is only ever (temporarily) set equal to `TRUE` via
#' [testthat::with_mocked_bindings()] in [testthat::test_that()] blocks. They
#' are additional unit tests in place to ensure it is always `FALSE` outside
#' of unit tests.
#'
#' @rdname constants
#' @keywords internal
.__LGL_DEBUG_FLAG <- FALSE
