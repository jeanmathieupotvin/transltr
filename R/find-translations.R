#' Placeholder
#'
#' Placeholder for future function [find_translations()].
#'
#' @export
find_translations <- function(path = getwd(), encoding = "UTF-8") {
    assert_chr1(path)
    assert_chr1(encoding)

    # Not super useful, but a little safer.
    path <- normalizePath(path, mustWork = FALSE)

    if (!utils::file_test("-d", path)) {
        stops("'path' does not exist or is not a directory.")
    }

    paths <- list.files(
        path         = path,
        pattern      = "\\.[Rr]$|(Rprofile)$",
        all.files    = TRUE,
        full.names   = TRUE,
        recursive    = TRUE,
        ignore.case  = FALSE,
        include.dirs = TRUE,
        no..         = TRUE)

    # TODO: this function should return a Translator object.
    # The following return value is temporary for debugging purposes.
    return(find_translations_in_files(paths, encoding))
}

find_translations_in_files <- function(paths = character(), encoding = "UTF-8") {
    return(unlist(lapply(paths, find_translations_in_file, encoding), FALSE))
}

find_translations_in_file <- function(path = "", encoding = "UTF-8") {
    return(find_translations_in_tokens(tokenize_file(path, encoding), path))
}

find_translations_in_tokens <- function(tokens = list(), path = "") {
    assert_list(tokens)

    if (anyNA(match(c("line1", "col1", "line2", "col2", "text"), names(tokens)))) {
        stops("'tokens' must contain 'line1' 'col1', 'line2', 'col2', and 'text' elements.")
    }

    src_text  <- tokens$text
    is_trans  <- is_translate_call(src_text)
    calls     <- lapply(src_text[is_trans], match_translate_call)
    lc_ranges <- lapply(tokens[c("line1", "col1", "line2", "col2")], `[`, i = is_trans)
    locations <- .mapply(location, lc_ranges, list(path = path))
    blocks    <- .mapply(as_block, list(calls, locations), list())
    return(do.call(merge_blocks, blocks))
}

tokenize_file <- function(path = "", encoding = "UTF-8") {
    # We use read_text() and parse(text = .) because
    # the former re-encodes source text to encoding.
    text   <- read_text(path, encoding)
    parsed <- parse(text = text, keep.source = TRUE, encoding = encoding)
    return(as.list(utils::getParseData(parsed, TRUE)))
}
