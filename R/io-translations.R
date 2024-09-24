#' Read and Write Translations Source Files
#'
#' Read [Translations Source Files] and parse them as [`Translator`][Translator]
#' objects, or convert [`Translator`][Translator] objects back to
#' [Translations Source Files].
#'
#' @template param-path
#'
#' @template param-encoding
#'
#' @param x An object of class [`Translator`][Translator].
#'
#' @returns
#' [read_translations()] returns a [`Translator`][Translator] object if `path`
#' points to a valid translations source file.
#'
#' [write_translations()] is not yet implemented and throws an error.
#'
#' @seealso [Translations Source Files]
#'
#' @rdname io-translations
#' @export
read_translations <- function(path = "", encoding = "UTF-8") {
    tsf_raw    <- read_text(path, encoding = encoding)
    tsf_parsed <- from_tsf(tsf_raw)

    # TODO: call Translator$new() once class is implemented.
    # Output below is temporary and for debugging purposes.
    return(tsf_parsed[c("header", "blocks")])
}

#' @rdname io-translations
#' @export
write_translations <- function(x, path = "") {
    return(.NotYetImplemented())
}
