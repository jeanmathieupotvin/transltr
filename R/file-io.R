#' Low-level text input and output
#'
#' [read_file()] and [write_file()] respectively wrap [base::readLines()] and
#' [base::writeLines()]. They further validate their arguments, normalize
#' file paths, and re-encode inputs to `UTF-8` before reading and writing.
#'
#' @param path A character string. A path to a file to read text from
#'   or write text to.
#'
#' @param encoding A character string. The file's underlying encoding.
#'   In almost all cases, this should be `UTF-8`. Text supplied in other
#'   encodings is internally re-encoded to `UTF-8` for portability.
#'
#' @param x A character vector. Lines of text to write.
#'
#' @returns
#' [read_text()] returns a character vector.
#'
#' [write_text()] is not yet implemented and throws an error.
#'
#' @rdname file-io
#' @family io
#' @keywords internal
read_text <- function(path = "", encoding = "UTF-8") {
    assert_chr1(path)
    assert_chr1(encoding)

    # Not super useful, but a little safer.
    path <- normalizePath(path, mustWork = FALSE)

    if (!utils::file_test("-f", path) ||
        !utils::file_test("-r", path)) {
        stops("'path' does not exist, is a directory, or is not readable.")
    }

    # This connection re-encodes input
    # to UTF-8 from supplied encoding.
    con <- file(path, "r", encoding = encoding)
    on.exit(close(con, "r"))

    # Setting encoding to UTF-8 explicitly marks
    # UTF-8 characters as such. Since input was
    # re-encoded to UTF-8 by file() above, this
    # is safe. No need to call enc2utf8().
    return(readLines(con, encoding = "UTF-8"))
}

# TODO: implement write_text() when write_translations() is being implemented.
#' @rdname file-io
#' @keywords internal
write_text <- function(x, path = "") {
    return(.NotYetImplemented())
}
