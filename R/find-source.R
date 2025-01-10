#' Find Source Text
#'
#' Find and extract source text that requires translation.
#'
#' [find_source()] and [find_source_in_files()] look for calls to [translate()]
#' in \R scripts and convert them to [`Text`][Text] objects via [as_text()].
#'
#' [find_source()] further constructs a [`Translator`][Translator] object from
#' the set of [`Text`][Text] objects. It can later be exported and imported
#' via [translator_write()] and [translator_read()] respectively.
#'
#' ## Methodology
#'
#' Extracting source text from source code involves performing usual parsing
#' operations. [find_source()] and [find_source_in_files()] go through these
#' steps to extract source text from a single \R script.
#'
#'   1. It is read with [text_read()].
#'   2. It is parsed with [parse()] and underlying tokens are extracted from
#'      parsed expressions with [utils::getParseData()].
#'   3. Each expression token (`expr`) is converted to language objects with
#'      [str2lang()]. Parsing errors and invalid expressions are silently
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
#' @template param-encoding
#'
#' @template param-strict
#'
#' @template param-id
#'
#' @template param-algorithm
#'
#' @template param-verbose
#'
#' @returns
#' [find_source()] returns an [`R6`][R6::R6] object of class
#' [`Translator`][Translator].
#'
#' [find_source_in_files()] returns a list of [`Text`][Text] objects. It may
#' contain duplicated elements, depending on the extracted contents.
#'
#' @seealso
#' [`Translator`][Translator],
#' [`Text`][Text],
#' [translate()],
#' [translator_read()],
#' [translator_write()]
#'
#' @examples
#' # Create a directory containing dummy R
#' # scripts for illustration purposes.
#' temp_dir   <- file.path(tempdir(TRUE), "find-source")
#' temp_files <- file.path(temp_dir, c("ex-script-1.R", "ex-script-2.R"))
#' dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
#'
#' cat(
#'   "translate('Not strict: Hello, world!')",
#'   "transltr::translate('Strict: Farewell, world!')",
#'   sep  = "\n",
#'   file = temp_files[[1L]])
#' cat(
#'   "transltr::translate('Strict: Hello, world!')",
#'   "translate('Not strict: Farewell, world!')",
#'   sep  = "\n",
#'   file = temp_files[[2L]])
#'
#' # Extract explicit calls to transltr::translate()
#' # from source scripts (strict = TRUE).
#' find_source(temp_dir, strict = TRUE, verbose = TRUE)
#' find_source_in_files(temp_files, strict = TRUE, verbose = TRUE)
#'
#' # Extract calls to any translate() function
#' # from source scripts (strict = FALSE).
#' find_source(temp_dir, strict = FALSE, verbose = TRUE)
#' find_source_in_files(temp_files, strict = FALSE, verbose = TRUE)
#'
#' @rdname find-source
#' @export
find_source <- function(
    path             = getwd(),
    encoding         = "UTF-8",
    strict           = TRUE,
    id               = uuid(),
    algorithm        = constant("algorithms"),
    native_languages = character(),
    verbose          = TRUE)
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

    texts <- find_source_in_files(paths, encoding, strict, algorithm, verbose)
    trans <- Translator$new(id, algorithm)

    storage.mode(native_languages) <- "list"
    do.call(trans$set_native_languages, native_languages)
    do.call(trans$set_texts, texts)
    return(trans)
}

#' @rdname find-source
#' @export
find_source_in_files <- function(
    paths     = character(),
    encoding  = "UTF-8",
    strict    = TRUE,
    algorithm = constant("algorithms"),
    verbose   = TRUE)
{
    assert_chr(paths)
    assert_lgl1(strict)
    assert_arg(algorithm, TRUE)
    assert_lgl1(verbose)

    texts <- lapply(paths, find_source_in_file, encoding, strict, algorithm, verbose)
    return(unlist(texts, FALSE))
}
