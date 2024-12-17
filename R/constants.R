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
#'
#' # NULL is returned if which has no corresponding entry.
#' constant("__undefined__")
#'
#' @rdname constants
#' @export
#' @keywords internal
constant <- function(which = c("concat", "empty", "unset", "unknown")) {
    assert_chr1(which)

    return(
        switch(which,
            concat  = " ",
            empty   = "<none>",
            unset   = "<unset>",
            unknown = "<unknown>",
            NULL))
}

#' @format `.__LGL_DEBUG_FLAG` is a logical value always equal to `FALSE`.
#'   It is only ever (temporarily) set equal to `TRUE` in unit tests via
#'   [testthat::with_mocked_bindings()] for development, and testing purposes.
#'   It is used to force errors that are otherwise hard, or impossible to test
#'   via usual methods. They are unit tests in place that ensures official
#'   releases are always shipped with this flag set to `FALSE`.
#' @rdname constants
#' @keywords internal
.__LGL_DEBUG_FLAG <- FALSE
