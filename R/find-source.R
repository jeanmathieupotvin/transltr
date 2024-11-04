#' Find Source Text
#'
#' Find, and extract source text that requires translation.
#'
#' [find_source()] and [find_source_in_files()] look for calls to [translate()]
#' in \R scripts, and convert them to [`Block`][Block] objects via [as_block()].
#'
#' [find_source()] further constructs a [`Translator`][Translator] object from
#' the set of [`Block`][Block] objects. It can later be exported, and imported
#' via [write_translations()] and [read_translations()] respectively.
#'
#' ## Limitations
#'
#' The current version of [`transltr`][transltr] can only handle **literal**
#' character vectors. This means it cannot process values passed to argument
#' `...` of [translate()] that depends on any state at runtime. There are
#' plans to lift this limitation in the future.
#'
#' @param path A non-empty and non-[NA][base::NA] character string. A path to
#'   a directory containing \R source scripts. All subdirectories are searched.
#'   Files that do not have a `.R`, or `.Rprofile` extension are skipped.
#'
#' @param paths A character vector of non-empty and non-[NA][base::NA] values.
#'   A set of paths to \R source scripts that must be searched.
#'
#' @param native_languages A named character vector of non-empty and
#'   non-[NA][base::NA] values. It can be empty. It is used to to construct
#'   a mapping of language codes to native language names. See field
#'   [`Translator$native_languages`][Translator] for more information.
#'
#' @template param-encoding
#'
#' @template param-strict
#'
#' @template param-id
#'
#' @template param-hash-algorithm
#'
#' @returns
#' [find_source()] returns an [`R6`][R6::R6] object of class
#' [`Translator`][Translator].
#'
#' [find_source_in_files()] returns a list of [`Block`][Block] objects. It may
#' contain duplicated elements, depending on the extracted contents.
#'
#' @seealso
#'   [`Translator`][Translator],
#'   [`Block`][Block],
#'   [translate()],
#'   [read_translations()],
#'   [write_translations()]
#'
#' @rdname find-source
#' @export
find_source <- function(
    path             = getwd(),
    encoding         = "UTF-8",
    strict           = TRUE,
    id               = uuid(),
    hash_algorithm   = get_hash_algorithms(),
    native_languages = character())
{
    assert_chr1(path)
    assert_chr(native_languages, TRUE)
    assert_named(native_languages)

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

    storage.mode(native_languages) <- "list"
    do.call(trans$set_native_languages, native_languages)
    do.call(trans$set_blocks, blocks)
    return(trans)
}

#' @rdname find-source
#' @export
find_source_in_files <- function(
    paths          = character(),
    encoding       = "UTF-8",
    strict         = TRUE,
    hash_algorithm = get_hash_algorithms())
{
    # encoding is validated by read_text() below.
    assert_chr(paths)
    assert_lgl1(strict)
    assert_arg(hash_algorithm, TRUE)

    blocks <- lapply(paths, find_source_in_file,
        encoding       = encoding,
        strict         = strict,
        hash_algorithm = hash_algorithm)

    return(unlist(blocks, FALSE))
}


#' Find Source Text
#'
#' @description
#' Find, and extract source text that requires translation from a single file,
#' or from a set of tokenized \R expressions stemming from [parse()].
#'
#' These functions are the *building blocks* of [find_source()] and
#' [find_source_in_files()]. They are not meant to be used by regular users.
#'
#' @param path A non-empty and non-[NA][base::NA] character string. A path to
#'   an \R source script.
#'
#' @param .tokens A [`data.frame`][data.frame()] returned by
#'   [utils::getParseData()]. This can be a subset of the latter, but must
#'   always contain columns `line1`, `col1`, `line2`, `col2`, and `text`.
#'
#' @param .path,.strict,.hash_algorithm Same as `path`, `strict`, and
#'   `hash_algorithm`, but not validated for maximum efficiency. They
#'   are checked by an higher-level parent function.
#'
#' @template param-encoding
#'
#' @template param-strict
#'
#' @template param-hash-algorithm
#'
#' @returns
#' [find_source_in_file()] and [find_source_in_exprs()] return a list of
#' [`Block`][Block] objects. It may contain duplicated elements, depending
#' on the extracted contents.
#'
#' [find_source_exprs()] returns the same output as [utils::getParseData()].
#' However, only `expr` tokens are returned.
#'
#' @seealso
#'   [`Block`][Block],
#'   [translate()],
#'   [find_source()]
#'
#' @rdname find-source-in-file
#' @keywords internal
find_source_in_file <- function(
    path           = "",
    encoding       = "UTF-8",
    strict         = TRUE,
    hash_algorithm = get_hash_algorithms())
{
    return(
        find_source_in_exprs(
            find_source_exprs(path, encoding),
            path,
            strict,
            hash_algorithm))
}

#' @rdname find-source-in-file
#' @keywords internal
find_source_in_exprs <- function(
    .tokens         = utils::getParseData(),
    .path           = "",
    .strict         = TRUE,
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
        map(as_block,
            x        = code[is_call],
            location = locations,
            moreArgs = list(
                strict         = .strict,
                hash_algorithm = .hash_algorithm,
                validate       = FALSE)))
}

#' @rdname find-source-in-file
#' @keywords internal
find_source_exprs <- function(path = "", encoding = "UTF-8") {
    # We use read_text() and parse(text = .) because
    # the former re-encodes source text to encoding.
    text   <- read_text(path, encoding)
    parsed <- parse(text = text, keep.source = TRUE, encoding = encoding)
    tokens <- utils::getParseData(parsed, TRUE)
    return(tokens[tokens$token == "expr", ])
}
