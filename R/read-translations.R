read_translations <- function(file_path = "", encoding = "UTF-8") {
    assert_chr1(file_path)
    assert_chr1(encoding)

    if (!utils::file_test("-f", file_path) ||
        !utils::file_test("-r", file_path)) {
        stops(
            "'file_path' either does not exist, ",
            "is a directory, or is not readable.")
    }

    file_con <- file(file_path, "r", encoding = encoding)
    on.exit(close(file_con, "r"))

    src_lines  <- readLines(file_con,  encoding = encoding)
    src_head   <- extract_src_header(src_lines)
    src_blocks <- extract_src_blocks(src_lines)

    header <- from_src_header(src_head)
    blocks <- lapply(src_blocks, from_src_block)

    # TODO: introduce translator() later once Translator class is implemented.
    # return(do.call(translator, list(blocks, file_path, header)))
    # Call to return() below is temporary for debugging purposes.
    return(c(header, blocks = list(blocks)))
}
