#' Read and write translations source files
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
#' @rdname src-translations
#' @export
read_translations <- function(path = "", encoding = "UTF-8") {
    src_lines <- read_text(path, encoding = encoding)
    src_parts <- split_tsf(src_lines)
    header    <- from_src_header(src_parts$HEADER)
    blocks    <- from_src_blocks(src_parts$BLOCKS, header$template_version)

    # TODO: call Translator$new() once class is implemented.
    # Output below is temporary and for debugging purposes.
    return(list(header = header, blocks = blocks))
}

#' @rdname src-translations
#' @export
write_translations <- function(x, path = "") {
    return(.NotYetImplemented())
}
