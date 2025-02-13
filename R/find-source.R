#' Find Source Text
#'
#' Find and extract source text that must be translated.
#'
#' @details
#' [find_source()] and [find_source_in_files()] look for calls to method
#' [`Translator$translate()`][Translator] in \R scripts and convert them
#' to [`Text`][Text] objects. The former further sets these resulting
#' objects into a [`Translator`][Translator] object. See argument `tr`.
#'
#' [find_source()] and [find_source_in_files()] work on a purely lexical basis.
#' The source code is parsed but never evaluated (aside from extracted literal
#' character vectors).
#'
#'   * The underlying [`Translator`][Translator] object is never evaluated and
#'     does not need to exist (placeholders may be used in the source code).
#'   * Only **literal** character vectors can be passed to arguments of
#'     method [`Translator$translate()`][Translator].
#'
#' ## Interfaces
#'
#' In some cases, it may not be desirable to call method
#' [`Translator$translate()`][Translator] directly. A custom function wrapping
#' (*interfacing*) this method may always be used as long as it has the same
#' [signature](https://en.wikipedia.org/wiki/Type_signature) as method
#' [`Translator$translate()`][Translator]. In other words, it must minimally
#' have two formal arguments: `...` and `source_lang`.
#'
#' Custom interfaces must be passed to [find_source()] and
#' [find_source_in_files()] for extraction purposes. Since these functions work
#' on a lexical basis, interfaces can be placeholders in the source code (non-
#' existent bindings) at the time these functions are called. However, they must
#' be bound to a function (ultimately) calling [`Translator$translate()`][Translator]
#' at runtime.
#'
#' Custom interfaces are passed to [find_source()] and [find_source_in_files()]
#' as [`name`][name] or [`call`][call] objects in a variety of ways. The most
#' straightforward way is to use [base::quote()]. See Examples below.
#'
#' ## Methodology
#'
#' [find_source()] and [find_source_in_files()] go through these steps to
#' extract source text from a single \R script.
#'
#'   1. It is read with [text_read()] and re-encoded to UTF-8 if necessary.
#'   2. It is parsed with [parse()] and underlying tokens are extracted from
#'      parsed expressions with [utils::getParseData()].
#'   3. Each expression (`expr`) token is converted to language objects with
#'      [str2lang()]. Parsing errors and invalid expressions are silently
#'      skipped.
#'   4. Valid [`call`][call()] objects stemming from step 3 are filtered with
#'      [is_source()].
#'   5. Calls to method [`Translator$translate()`][Translator] or to `interface`
#'      stemming from step 4 are coerced to [`Text`][Text] objects with
#'      [as_text()].
#'
#' These steps are repeated for each \R script. [find_source()] further merges
#' all resulting [`Text`][Text] objects into a coherent set with [merge_texts()]
#' (identical source code is merged into single [`Text`][Text] entities).
#'
#' Extracted character vectors are always normalized for consistency (at step
#' 5). See [normalize()] for more information.
#'
#' ## Limitations
#'
#' The current version of [`transltr`][transltr] can only handle **literal**
#' character vectors. This means it cannot resolve non-trivial expressions
#' that depends on a *state*. All values passed to argument `...` of method
#' [`Translator$translate()`][Translator] must yield character vectors
#' (trivially).
#'
#' @param path A non-empty and non-NA character string. A path to a directory
#'   containing \R source scripts. All subdirectories are searched. Files that
#'   do not have a `.R`, or `.Rprofile` extension are skipped.
#'
#' @param paths A character vector of non-empty and non-NA values. A set of
#'   paths to \R source scripts that must be searched.
#'
#' @param tr A [`Translator`][Translator] object.
#'
#' @template param-encoding
#'
#' @template param-algorithm
#'
#' @template param-verbose
#'
#' @template param-interface
#'
#' @returns
#' [find_source()] returns an [`R6`][R6::R6] object of class
#' [`Translator`][Translator]. If an existing [`Translator`][Translator]
#' object is passed to `tr`, it is modified in place and returned.
#'
#' [find_source_in_files()] returns a list of [`Text`][Text] objects. It may
#' contain duplicated elements, depending on the extracted contents.
#'
#' @seealso
#' [`Translator`][Translator],
#' [`Text`][Text],
#' [normalize()],
#' [translator_read()],
#' [translator_write()],
#' [base::quote()],
#' [base::call()],
#' [base::as.name()]
#'
#' @examples
#' # Create a directory containing dummy R scripts for illustration purposes.
#' temp_dir   <- file.path(tempdir(TRUE), "find-source")
#' temp_files <- file.path(temp_dir, c("ex-script-1.R", "ex-script-2.R"))
#' dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
#'
#' cat(
#'   "tr$translate('Hello, world!')",
#'   "tr$translate('Farewell, world!')",
#'   sep  = "\n",
#'   file = temp_files[[1L]])
#' cat(
#'   "tr$translate('Hello, world!')",
#'   "tr$translate('Farewell, world!')",
#'   sep  = "\n",
#'   file = temp_files[[2L]])
#'
#' # Extract calls to method Translator$translate().
#' find_source(temp_dir)
#' find_source_in_files(temp_files)
#'
#' # Use custom functions.
#' # For illustrations purposes, assume the package
#' # exports an hypothetical translate() function.
#' cat(
#'   "translate('Hello, world!')",
#'   "transtlr::translate('Farewell, world!')",
#'   sep  = "\n",
#'   file = temp_files[[1L]])
#' cat(
#'   "translate('Hello, world!')",
#'   "transltr::translate('Farewell, world!')",
#'   sep  = "\n",
#'   file = temp_files[[2L]])
#'
#' # Extract calls to translate() and transltr::translate().
#' # Since find_source() and find_source_in_files() work on
#' # a lexical basis, these are always considered to be two
#' # distinct functions. They also don't need to exist in the
#' # R session calling find_source() and find_source_in_files().
#' find_source(temp_dir, interface = quote(translate))
#' find_source_in_files(temp_files, interface = quote(transltr::translate))
#'
#' @rdname find-source
#' @export
find_source <- function(
    path      = ".",
    encoding  = "UTF-8",
    verbose   = getOption("transltr.verbose", TRUE),
    tr        = translator(),
    interface = NULL)
{
    assert_chr1(path)

    if (!utils::file_test("-d", normalizePath(path, mustWork = FALSE))) {
        stops("'path' does not exist or is not a directory.")
    }
    if (!is_translator(tr)) {
        stops("'tr' must be a 'Translator' object.")
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

    texts <- find_source_in_files(paths,
        encoding  = encoding,
        verbose   = verbose,
        algorithm = tr$algorithm,
        interface = interface)

    do.call(tr$set_texts, texts)
    return(tr)
}

#' @rdname find-source
#' @export
find_source_in_files <- function(
    paths     = character(),
    encoding  = "UTF-8",
    verbose   = getOption("transltr.verbose", TRUE),
    algorithm = algorithms(),
    interface = NULL)
{
    assert_chr(paths)
    assert_lgl1(verbose)
    assert_arg(algorithm, TRUE)

    if (!is.null(interface) &&
        !is.name(interface) && (
        !is.call(interface) ||
        !identical(interface[[1L]], quote(`::`)))) {
        stops(
            "'interface' must be a 'name', a 'call' object, or 'NULL'.\n",
            "Calls must be to operator `::`, i.e. 'pkg::fun'.")
    }

    texts <- lapply(paths, find_source_in_file,
        encoding  = encoding,
        verbose   = verbose,
        algorithm = algorithm,
        interface = interface)

    return(unlist(texts, FALSE))
}
