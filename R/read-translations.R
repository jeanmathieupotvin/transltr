read_translations <- function(path = "", encoding = "UTF-8") {
    src_lines <- read_text(path, encoding = encoding)
    head      <- from_src_header(extract_src_header(src_lines))
    blocks    <- from_src_blocks(extract_src_blocks(src_lines))

    # TODO: call Translator$new() once class is implemented.
    # Output below is temporary and for debugging purposes.
    return(list(header = header, blocks = blocks))
}
