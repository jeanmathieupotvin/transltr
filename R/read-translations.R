read_translations <- function(file_path = "", encoding = "UTF-8") {
    assert_chr1(file_path)
    assert_chr1(encoding)

    if (!utils::file_test("-f", file_path) ||
        !utils::file_test("-r", file_path)) {
        stops("'file_path' does not exist, is a directory, or is not readable.")
    }

    file_con <- file(file_path, "r", encoding = encoding)
    on.exit(close(file_con, "r"))

    src_lines  <- readLines(file_con, encoding = encoding)
    src_head   <- from_src_header(extract_src_header(src_lines))
    src_blocks <- from_src_blocks(extract_src_blocks(src_lines))

    # TODO: introduce translator() later once Translator class is implemented.
    # Call to return() below is temporary for debugging purposes.
    return(c(header = header, blocks = blocks))
}
