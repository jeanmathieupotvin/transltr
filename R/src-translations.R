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
#' @param readme A logical. Should a `README` Markdown file also be generated?
#'   It contains portable instructions and details on translations source files.
#'   If `TRUE`, it is generated in the same directory as `path`.
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
    header    <- from_src_header(extract_src_header(src_lines))
    blocks    <- from_src_blocks(extract_src_blocks(src_lines))

    # TODO: call Translator$new() once class is implemented.
    # Output below is temporary and for debugging purposes.
    return(list(header = header, blocks = blocks))
}

#' @rdname src-translations
#' @export
write_translations <- function(x, path = "", readme = TRUE) {
    return(.NotYetImplemented())
}
