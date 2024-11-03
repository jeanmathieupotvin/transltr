#' Placeholder
#'
#' Placeholder.
#'
#' @param path Placeholder.
#' @param encoding Placeholder.
#'
#' @template param-hash-algorithm
#'
#' @rdname find-source
#' @export
find_source <- function(
    path           = getwd(),
    encoding       = "UTF-8",
    strict         = FALSE,
    id             = uuid(),
    hash_algorithm = get_hash_algorithms(),
    ...)
{
    assert_chr1(path)

    if (!utils::file_test("-d", path <- normalizePath(path, mustWork = FALSE))) {
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

    blocks <- find_source_in_files(paths, encoding, strict, hash_algorithm)
    trans  <- Translator$new(id, hash_algorithm)
    trans$set_native_languages(...)
    do.call(trans$set_blocks, blocks)
    return(trans)
}

#' @rdname find-source
#' @export
find_source_in_files <- function(
    paths          = character(),
    encoding       = "UTF-8",
    strict         = FALSE,
    hash_algorithm = get_hash_algorithms())
{
    # encoding is validated by read_text() below.
    assert_chr(paths)
    assert_lgl1(strict)
    assert_arg(hash_algorithm)

    blocks <- lapply(paths, find_source_in_file,
        encoding       = encoding,
        strict         = strict,
        hash_algorithm = hash_algorithm)

    return(unlist(blocks, FALSE))
}


#' Placeholder
#'
#' Placeholder.
#'
#' @rdname find-source-in-file
#' @keywords internal
find_source_in_file <- function(
    path           = "",
    encoding       = "UTF-8",
    strict         = FALSE,
    hash_algorithm = get_hash_algorithms())
{
    return(
        find_source_in_exprs(
            parse_file(path, encoding),
            path,
            strict,
            hash_algorithm))
}

#' @rdname find-source-in-file
#' @keywords internal
find_source_in_exprs <- function(
    .tokens         = list(),
    .path           = "",
    .strict         = FALSE,
    .hash_algorithm = get_hash_algorithms())
{
    code      <- lapply(.tokens$text, str2lang)
    is_call   <- vapply_1l(code, is_translate_call, .strict = .strict)
    locations <- map(location, moreArgs = list(path = .path),
        line1 = .tokens[is_call, "line1"],
        col1  = .tokens[is_call, "col1"],
        line2 = .tokens[is_call, "line2"],
        col2  = .tokens[is_call, "col2"])

    return(
        map(as_block, x = code[is_call], location = locations, moreArgs = list(
            strict         = .strict,
            hash_algorithm = .hash_algorithm,
            validate       = FALSE)))
}

#' @rdname find-source-in-file
#' @keywords internal
parse_file <- function(path = "", encoding = "UTF-8") {
    # We use read_text() and parse(text = .) because
    # the former re-encodes source text to encoding.
    text   <- read_text(path, encoding)
    parsed <- parse(text = text, keep.source = TRUE, encoding = encoding)
    tokens <- utils::getParseData(parsed, TRUE)
    return(tokens[tokens$token == "expr", ])
}
