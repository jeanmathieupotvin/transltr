#' Find Source Text
#'
#' Find, and extract source text that requires translation.
#'
#' [find_source()] and [find_source_in_files()] look for calls to [translate()]
#' in \R scripts, and convert them to [`Text`][Text] objects via [as_text()].
#'
#' [find_source()] further constructs a [`Translator`][Translator] object from
#' the set of [`Text`][Text] objects. It can later be exported, and imported
#' via [translator_write()] and [translator_read()] respectively.
#'
#' ## Methodology
#'
#' Extracting source text from source code involves performing usual parsing
#' operations. [find_source()] and [find_source_in_files()] go through these
#' steps to extract source text from a single \R script.
#'
#'   1. It is read with [text_read()].
#'   2. It is parsed with [parse()], and underlying tokens are extracted from
#'      parsed expressions with [utils::getParseData()].
#'   3. Each expression token (`expr`) is converted to language objects with
#'      [str2lang()]. Parsing errors, and invalid expressions are silently
#'      skipped.
#'   4. Valid [`call`][call()] objects stemming from step 3 are filtered with
#'      [is_translate_call()].
#'   5. Calls to [translate()] stemming from step 4 are coerced to
#'      [`Text`][Text] objects with [as_text()].
#'
#' [find_source()] further constructs a [`Translator`][Translator] object from
#' [`Text`][Text] objects stemming from step 5.
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
#' @param verbose A non-[NA][base::NA] logical value. Should basic information
#'   on extracted source texts be outputted?
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
#' [find_source_in_files()] returns a list of [`Text`][Text] objects. It may
#' contain duplicated elements, depending on the extracted contents.
#'
#' @seealso
#'   [`Translator`][Translator],
#'   [`Text`][Text],
#'   [translate()],
#'   [translator_read()],
#'   [translator_write()]
#'
#' @rdname find-source
#' @export
find_source <- function(
    path             = getwd(),
    encoding         = "UTF-8",
    strict           = TRUE,
    id               = uuid(),
    hash_algorithm   = hash_algorithms(),
    native_languages = character(),
    verbose          = FALSE)
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

    texts <- find_source_in_files(paths, encoding, strict, hash_algorithm, verbose)
    trans <- Translator$new(id, hash_algorithm)

    storage.mode(native_languages) <- "list"
    do.call(trans$set_native_languages, native_languages)
    do.call(trans$set_texts, texts)
    return(trans)
}

#' @rdname find-source
#' @export
find_source_in_files <- function(
    paths          = character(),
    encoding       = "UTF-8",
    strict         = TRUE,
    hash_algorithm = hash_algorithms(),
    verbose        = FALSE)
{
    # encoding is validated by text_read() below.
    assert_chr(paths)
    assert_lgl1(strict)
    assert_arg(hash_algorithm, TRUE)
    assert_lgl1(verbose)

    texts <- lapply(paths, find_source_in_file,
        encoding,
        strict,
        hash_algorithm,
        verbose)
    return(unlist(texts, FALSE))
}


#' Find Source Text in Expression Tokens
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
#' @param .verbose Same as argument `verbose` of [find_source()], but not
#'   validated for maximum efficiency. It is checked by an higher-level parent
#'   function.
#'
#' @template param-encoding
#'
#' @template param-strict
#'
#' @template param-hash-algorithm
#'
#' @details
#' [find_source_in_exprs()] silently skips parsing errors. See [find_source()]
#' for more information.
#'
#' @returns
#' [find_source_in_file()] and [find_source_in_exprs()] return a list of
#' [`Text`][Text] objects. It may contain duplicated elements, depending
#' on the extracted contents.
#'
#' [find_source_exprs()] returns the same output as [utils::getParseData()].
#' However, only `expr` tokens are returned.
#'
#' @seealso
#'   [`Text`][Text],
#'   [translate()],
#'   [find_source()]
#'
#' @rdname find-source-in-file
#' @keywords internal
find_source_in_file <- function(
    path           = "",
    encoding       = "UTF-8",
    strict         = TRUE,
    hash_algorithm = hash_algorithms(),
    .verbose       = FALSE)
{
    tokens <- find_source_exprs(path, encoding)
    texts  <- find_source_in_exprs(tokens, path, strict, hash_algorithm)

    if (.verbose) {
        cat(sep = "\n", sprintf(
            "Extracted %i source text(s) from '%s'.",
            length(texts),
            gsub(getwd(), ".", path)))
    }

    return(texts)
}

#' @rdname find-source-in-file
#' @keywords internal
find_source_in_exprs <- function(
    .tokens         = utils::getParseData(),
    .path           = "",
    .strict         = TRUE,
    .hash_algorithm = hash_algorithms())
{
    # Parsing errors are skipped silently. This is required whenever
    # native pipes are used. They introduce placeholders (_) in expr
    # tokens, a special constant that makes no sense outside of the
    # full context. Some tokens are sub-exprs and lack the former,
    # which yields an error. tryCatch() introduces a non-negligible
    # overhead, but it is the only viable solution.
    code      <- lapply(.tokens$text, \(x) tryCatch(str2lang(x), error = \(c) NULL))
    is_call   <- vapply_1l(code, is_translate_call, .strict = .strict)
    locations <- map(location, moreArgs = list(path = .path),
        line1 = .tokens[is_call, "line1"],
        col1  = .tokens[is_call, "col1"],
        line2 = .tokens[is_call, "line2"],
        col2  = .tokens[is_call, "col2"])

    return(
        map(as_text,
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
    # We use text_read() and parse(text = .) because
    # the former re-encodes source text to encoding.
    text   <- text_read(path, encoding)
    parsed <- parse(text = text, keep.source = TRUE, encoding = encoding)
    tokens <- utils::getParseData(parsed, TRUE)
    return(tokens[tokens$token == "expr", ])
}
