#' Internal Constants
#'
#' Fetch a constant used by [`transltr`][transltr]. Constants ensure
#' consistency among all features of the package.
#'
#' @param which A non-empty and non-[NA][base::NA] character string. The name
#'   of the constant to fetch. See below for defined values.
#'
#' @returns
#' [constant()] returns the requested constant, or `NULL` if it is unavailable.
#'
#' | `which`         | **Shape**      | **Value**                                     |
#' | --------------- | -------------- | --------------------------------------------- |
#' | `algorithms`    | `character(2)` | `c("sha1", "utf8")`                           |
#' | `concat`        | `character(1)` | `" "`                                         |
#' | `empty`         | `character(1)` | `"<empty>"`                                   |
#' | `empty-list`    | `character(1)` | `"<empty list>"`                              |
#' | `null`          | `character(1)` | `"<null>"`                                    |
#' | `unset`         | `character(1)` | `"<unset>"`                                   |
#' | `unknown`       | `character(1)` | `"<unknown>"`                                 |
#' | `untranslated`  | `character(1)` | `"# Insert a translation here."`              |
#' | `range-sprintf` | `character(1)` | `"Ln %s, Col %s @ Ln %s, Col %s"`             |
#' | `range-format`  | `character(1)` | `"Ln <int>, Col <int> @ Ln <int>, Col <int>"` |
#' | `range-pattern` | `character(1)` | `"^Ln[ \t]*([0-9.]+),[ \t]*Col[ \t]*..."`     |
#'
#' @section Algorithms returned by `constant("algorithms")`:
#' These algorithms map a character string to another character string of
#' hexadecimal characters highly likely to be unique. The latter is typically
#' much shorter and is used to uniquely identify a source text (and the
#' underlying source language). Two algorithms are available.
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
#' small inputs and emits hashes with a width porportional to the underlying
#' input's length. It is used for testing purposes internally.
#'
#' @rdname constants
#' @keywords internal
constant <- function(which = "") {
    assert_chr1(which)

    return(
        switch(which,
            algorithms      = c("sha1", "utf8"),
            concat          = " ",
            empty           = "<empty>",
            `empty-list`    = "<empty list>",
            null            = "<null>",
            unset           = "<unset>",
            unknown         = "<unknown>",
            untranslated    = "# Insert a translation here.",
            `range-sprintf` = "Ln %s, Col %s @ Ln %s, Col %s",
            `range-format`  = "`Ln <int>, Col <int> @ Ln <int>, Col <int>`",
            `range-pattern` = "^Ln[ \t]*([0-9.]+),[ \t]*Col[ \t]*([0-9.]+)[ \t]*@[ \t]*Ln[ \t]*([0-9.]+),[ \t]*Col[ \t]*([0-9.]+)$",
            NULL))
}


# An internal logical value always equal to `FALSE` used to force errors that
# are hard, or (almost) impossible to test otherwise. It is only temporarily
# set equal to `TRUE` via [testthat::with_mocked_bindings()].
.__LGL_DEBUG_FLAG <- FALSE
