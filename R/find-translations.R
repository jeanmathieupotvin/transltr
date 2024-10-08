#' Placeholder
#'
#' Placeholder for future function [find_translations()].
#'
#' @export
find_translations <- function(
    path           = getwd(),
    encoding       = "UTF-8",
    hash_algorithm = get_hash_algorithms())
{
    # We validate this arg here because it is
    # directly used before being passed down.
    assert_chr1(path)

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
    # The following value is temporary for debugging purposes.
    return(find_translations_in_files(paths, encoding, hash_algorithm))
}

find_translations_in_files <- function(
    paths          = character(),
    encoding       = "UTF-8",
    hash_algorithm = get_hash_algorithms())
{
    assert_chr(paths)
    blocks <- unlist(
        lapply(paths, find_translations_in_file, encoding, hash_algorithm),
        recursive = FALSE)

    if (!length(blocks)) {
        return(list())
    }

    # Extracting hash_algorithm from first block
    # allows us to bypass asser_arg() and defer
    # validation to the lowest possible level.
    return(
        do.call(merge_blocks, c(
            blocks,
            hash_algorithm = blocks[[1L]]$hash_algorithm)))
}

find_translations_in_file <- function(
    path           = "",
    encoding       = "UTF-8",
    hash_algorithm = get_hash_algorithms())
{
    tokens <- tokenize_file(path, encoding)
    return(find_translations_in_tokens(tokens, path, hash_algorithm))
}

find_translations_in_tokens <- function(
    tokens         = list(),
    path           = "",
    hash_algorithm = get_hash_algorithms())
{
    assert_list(tokens)

    if (anyNA(match(c("line1", "col1", "line2", "col2", "text"), names(tokens)))) {
        stops("'tokens' must contain 'line1' 'col1', 'line2', 'col2', and 'text' elements.")
    }

    src_text  <- tokens$text
    is_trans  <- is_translate_call(src_text)
    calls     <- lapply(src_text[is_trans], match_translate_call)
    lc_ranges <- lapply(tokens[c("line1", "col1", "line2", "col2")], `[`, i = is_trans)
    locations <- .mapply(location, lc_ranges, list(path = path))

    return(
        .mapply(as_block,
            list(calls, locations),
            list(hash_algorithm = hash_algorithm)))
}

tokenize_file <- function(path = "", encoding = "UTF-8") {
    # We use read_text() and parse(text = .) because
    # the former re-encodes source text to encoding.
    text   <- read_text(path, encoding)
    parsed <- parse(text = text, keep.source = TRUE, encoding = encoding)
    return(as.list(utils::getParseData(parsed, TRUE)))
}
