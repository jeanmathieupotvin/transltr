#' Parameters and Constants
#'
#' Helper functions that return valid values for various parameters and
#' components of package [`transltr`][transltr].
#'
#' @section Hashing algorithms:
#' Package [`transltr`][transltr] offers two methods.
#'
#'   * Method `sha1` corresponds to SHA-1 (Secure Hash Algorithm 1),
#'     a cryptographic hashing function. While it is now superseded by more
#'     secure variants (SHA-256, SHA-512, etc.), it is still useful for non-
#'     sensitive purposes. It is fast, collision-resistant, and may handle
#'     very large inputs. It outputs strings of 40 hexadecimal characters.
#'
#'   * Method `utf8` is an **experimental hashing method** derived from the
#'     cumulative sum of code points. It is slightly faster that `sha1` for
#'     smaller inputs, and outputs shorter integers (as strings) of variable
#'     lengths porportional to the input's length. It should be reserved for
#'     expert use only.
#'
#' @returns
#' [get_template_versions()] returns an integer vector.
#'
#' [get_hash_algorithms()] returns a character vector.
#'
#' [get_generated_by()] returns a character string.
#'
#' [get_generated_on()] is just a semantic alias for [utc()] and returns
#' whatever it returns.
#'
#' @seealso [utc()]
#'
#' @rdname transltr-params
#' @keywords internal
get_template_versions <- function() {
    return(1L)
}

#' @rdname transltr-params
#' @keywords internal
get_hash_algorithms <- function() {
    return(c("sha1", "utf8"))
}

#' @rdname transltr-params
#' @keywords internal
get_generated_by <- function() {
    return("R package transltr 0.0.1.9003")
}

#' @include utc.R
#' @rdname transltr-params
#' @keywords internal
get_generated_on <- utc
