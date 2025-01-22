#' Read and Write Text
#'
#' [text_read()] and [text_write()] respectively wrap [base::readLines()] and
#' [base::writeLines()]. They further validate their arguments, normalize
#' file paths and re-encode inputs to `UTF-8` before reading and writing.
#'
#' @template param-path
#'
#' @template param-encoding
#'
#' @param x A character vector. Lines of text to write. Its current encoding is
#'   given by `encoding`.
#'
#' @returns
#' [text_read()] returns a character vector.
#'
#' [text_write()] returns `NULL`, invisibly.
#'
#' @seealso
#' [readLines()],
#' [writeLines()],
#' [iconv()]
#'
#' @rdname text-io
#' @keywords internal
text_read <- function(path = "", encoding = "UTF-8") {
    assert_chr1(path)
    assert_chr1(encoding)

    # Not super useful, but a little safer.
    path_norm <- normalizePath(path, mustWork = FALSE)

    if (!utils::file_test("-f", path_norm) ||
        !utils::file_test("-r", path_norm)) {
        stopf(
            "'path' '%s' does not exist, is a directory, or is not readable.",
            path)
    }

    # This connection re-encodes input
    # to UTF-8 from supplied encoding.
    con <- file(path_norm, "r", encoding = encoding)
    on.exit(close(con, "r"))

    # Setting encoding to UTF-8 explicitly marks
    # UTF-8 characters as such. Since input was
    # re-encoded to UTF-8 by file() above, this
    # is safe. No need to call enc2utf8().
    return(readLines(con, encoding = "UTF-8"))
}

#' @rdname text-io
#' @keywords internal
text_write <- function(x = character(), path = "", encoding = "UTF-8") {
    assert_chr(x)
    assert_chr1(path)
    assert_chr1(encoding)

    # Not super useful, but a little safer.
    path_norm <- normalizePath(path, mustWork = FALSE)

    if (utils::file_test("-d", path_norm) || (
        utils::file_test("-f", path_norm) && !utils::file_test("-w", path_norm))) {
        stopf("'path' '%s' is a directory, or is not writable.", path)
    }

    con <- file(path_norm, "wb", encoding = encoding)
    on.exit(close(con, "wb"))

    # Character elements are explicitly re-encoded to UTF-8
    # before being written. The resulting text is written as
    # bytes directly (this is faster). In almost all cases,
    # it should already be in UTF-8. iconv() is used because
    # enc2utf8() handles fewer encodings.
    return(
        writeLines(
            iconv(x, encoding, "UTF-8"),
            con      = con,
            sep      = "\n",
            useBytes = TRUE))
}
