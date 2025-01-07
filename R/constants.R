#' Internal Constants
#'
#' Fetch a constant used by [`transltr`][transltr]. Constants ensure
#' consistency among all features of the package.
#'
#' @param which A non-empty and non-[NA][base::NA] character string. The name
#'   of the constant to fetch.
#'
#' @returns
#' [constant()] returns the requested constant, or `NULL` if it is unavailable.
#' See Examples below.
#'
#' @section Hashing Algorithms:
#' Hashing algorithms map an arbitrary character string to a shorter string of
#' hexadecimal characters. It typically has a fixed width and is highly likely
#' to be unique. Available methods are listed by `constant("algorithms")`.
#'
#' ## Secure Hash Algorithm 1
#'
#' Method `sha1` corresponds to SHA-1 (Secure Hash Algorithm version 1), a
#' cryptographic hashing function. While it is now superseded by more secure
#' variants (SHA-256, SHA-512, etc.), it is still useful for non-sensitive
#' purposes. It is fast, collision-resistant, and may handle very large inputs.
#' It emits strings of 40 hexadecimal characters.
#'
#' ## Cumulative UTF-8 Sum
#'
#' `r lifecycle::badge("experimental")`
#'
#' **This method is experimental. Use with caution.**
#'
#' Method `utf8` is a simple method derived from cumulative sums of UTF-8 code
#' points (converted to integers). It is slightly faster than method `sha1` for
#' small inputs, and emits hashes with a width porportional to the underlying
#' input's length.
#'
#' Strictly speaking, this method is not a hashing algorithm per se. Instead,
#' it should be viewed as an identification algorithm that is highly likely to
#' produce different values for different inputs.
#'
#' @examples
#' transltr:::constant("algorithms")    ## Outputs c("sha1", "utf8")
#' transltr:::constant("concat")        ## Outputs " "
#' transltr:::constant("empty")         ## Outputs "<empty>"
#' transltr:::constant("null")          ## Outputs "<null>"
#' transltr:::constant("unset")         ## Outputs "<unset>"
#' transltr:::constant("unknown")       ## Outputs "<unknown>"
#' transltr:::constant("untranslated")  ## Outputs "# Erase this comment and provide a translation."
#'
#' # NULL is returned if which has no corresponding entry.
#' transltr:::constant("__undefined__")
#'
#' @rdname constants
#' @keywords internal
constant <- function(which = "") {
    assert_chr1(which)

    return(
        switch(which,
            algorithms   = c("sha1", "utf8"),
            concat       = " ",
            empty        = "<empty>",
            null         = "<null>",
            unset        = "<unset>",
            unknown      = "<unknown>",
            untranslated = "# Erase this comment and provide a translation.",
            NULL))
}


# An internal logical value always equal to `FALSE` used to force errors that
# are hard, or (almost) impossible to test otherwise. It is only temporarily
# set equal to `TRUE` via [testthat::with_mocked_bindings()].
.__LGL_DEBUG_FLAG <- FALSE
