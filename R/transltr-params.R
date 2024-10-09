#' Parameters
#'
#' Helper functions that return valid values for various parameters and
#' components of package [`transltr`][transltr].
#'
#' @template param-hash-algorithm
#'
#' @details
#' From the users' perspective, the hash length correspond to the number of
#' hexadecimal characters for convenience. Internally, it corresponds to a
#' number of bytes.
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
#' @family internal parameters
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
    return("R package transltr 0.0.1.9001")
}

#' @include utc.R
#' @rdname transltr-params
#' @keywords internal
get_generated_on <- utc
