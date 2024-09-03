#' Parameters
#'
#' Helper functions that return valid values for various parameters and
#' components of package [`transltr`][transltr].
#'
#' @param hash_algorithm An algorithm to use for hashing purposes. It must
#'   be a value returned by [get_hash_algorithms()].
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
#' [get_hash_length_range()] returns a named integer vector of length 2 which
#' represents the range of valid values for the hashes' length (**in bytes**).
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
    return("blake2b")
}

#' @rdname transltr-params
#' @keywords internal
get_hash_length_range <- function(hash_algorithm = get_hash_algorithms()) {
    assert_arg(hash_algorithm, TRUE)

    return(
        switch(hash_algorithm,
            "blake2b" = c(min = 8L, max = 32L),
            NULL))
}
