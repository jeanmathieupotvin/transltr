#' Find Source Text
#'
#' Find and extract source text that must be translated.
#'
#' @details
#' [find_source_text()] and [find_source_text_in_files()] look for calls to
#' method [`Translator$translate()`][Translator] in \R scripts and convert
#' them to [`SourceText`][SourceText] objects. The former further registers
#' them into a [`Translator`][Translator] object.
#'
#' [find_source_text()] and [find_source_text_in_files()] work on a purely
#' lexical basis. The source code is parsed but never evaluated (aside from
#' extracted literal character vectors).
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
#' Custom interfaces must be passed to [find_source_text()] and
#' [find_source_text_in_files()] for extraction purposes. Since these functions
#' work on a lexical basis, interfaces can be placeholders in the source code
#' (non-existent bindings) at the time these functions are called. However,
#' they must be bound to a function (ultimately) calling
#' [`Translator$translate()`][Translator] at runtime.
#'
#' Custom interfaces are passed to [find_source_text()] and
#' [find_source_text_in_files()] as [`name`][name] or [`call`][call] objects.
#' The most straightforward way is to use [base::quote()]. See Examples below.
#'
#' ## Methodology
#'
#' [find_source_text()] and [find_source_text_in_files()] go through these steps to
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
#'      stemming from step 4 are coerced to [`SourceText`][SourceText] objects with
#'      [as_text()].
#'
#' These steps are repeated for each \R script. [find_source_text()] further merges
#' all resulting [`SourceText`][SourceText] objects into a coherent set with [merge_texts()]
#' (identical source code is merged into single [`SourceText`][SourceText] entities).
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
#' @template param-verbose
#'
#' @template param-interface
#'
#' @returns
#' [find_source_text()] returns an [`R6`][R6::R6] object of class
#' [`Translator`][Translator]. If an existing [`Translator`][Translator]
#' object is passed to `tr`, it is modified in place and returned.
#'
#' [find_source_text_in_files()] returns a list of [`SourceText`][SourceText] objects. It may
#' contain duplicated elements, depending on the extracted contents.
#'
#' @seealso
#' [`Translator`][Translator],
#' [`SourceText`][SourceText],
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
#' find_source_text(temp_dir)
#' find_source_text_in_files(temp_files)
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
#' # Since find_source_text() and find_source_text_in_files() work on
#' # a lexical basis, these are always considered to be two
#' # distinct functions. They also don't need to exist in the
#' # R session calling find_source_text() and find_source_text_in_files().
#' find_source_text(temp_dir, interface = quote(translate))
#' find_source_text_in_files(temp_files, interface = quote(transltr::translate))
#'
#' @rdname find-source-text
#' @export
find_source_text <- function(
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

    texts <- find_source_text_in_files(paths,
        encoding  = encoding,
        verbose   = verbose,
        interface = interface)

    tr$add_source_texts(.list = texts)
    return(tr)
}

#' @rdname find-source-text
#' @export
find_source_text_in_files <- function(
    paths     = character(),
    encoding  = "UTF-8",
    verbose   = getOption("transltr.verbose", TRUE),
    interface = NULL)
{
    assert_chr(paths)
    assert_lgl1(verbose)

    if (!is.null(interface) &&
        !is.name(interface) && (
        !is.call(interface) ||
        !identical(interface[[1L]], quote(`::`)))) {
        stops(
            "'interface' must be a 'name', a 'call' object, or 'NULL'.\n",
            "Calls must be to operator `::`, i.e. 'pkg::fun'.")
    }

    texts <- lapply(paths, find_source_text_in_file,
        encoding  = encoding,
        verbose   = verbose,
        interface = interface)

    return(unlist(texts, FALSE))
}

#' Find Source Text in Expressions
#'
#' @description
#' Find and extract source text that must be translated from a single file
#' or a set of \R `expr` tokens.
#'
#' **Arguments listed below are not explicitly validated for efficiency.**
#'
#' @param path A non-empty and non-NA character string. A path to an \R source
#'   script.
#'
#' @param tokens A [`data.frame`][data.frame()] returned by
#'   [utils::getParseData()]. It must always minimally contain
#'   columns `line1`, `col1`, `line2`, `col2`, and `text`.
#'
#' @param x Any \R object.
#'
#' @template param-encoding
#'
#' @template param-verbose
#'
#' @template param-interface
#'
#' @details
#' [find_source_text_in_exprs()] silently skips parsing errors. See [find_source()]
#' for more information.
#'
#' [is_source()] checks if an object conceptually represents a source text.
#' This can either be
#'
#'   * a [`call`][call] to method [`Translator$translate()`][Translator] **or**
#'   * a [`call`][call] to a custom function referenced by `interface`.
#'
#' Calls to method [`Translator$translate()`][Translator] that include
#' [`...`][dots] in their argument(s) are ignored. Such calls are part
#' of the definition of a custom `interface` and should not be extracted.
#'
#' @returns
#' [find_source_text_in_file()] and [find_source_text_in_exprs()] return a list of
#' [`Text`][Text] objects. It may contain duplicated elements, depending
#' on the extracted contents.
#'
#' [find_source_text_exprs()] returns a subset of the output of
#' [utils::getParseData()]. Only `expr` tokens are returned.
#'
#' [is_source()] returns a logical value.
#'
#' @seealso
#' [`Text`][Text],
#' [find_source()],
#' [utils::getParseData()]
#'
#' @rdname find-source-text-in-exprs
#' @keywords internal
find_source_text_in_file <- function(
    path      = "",
    encoding  = "UTF-8",
    verbose   = getOption("transltr.verbose", TRUE),
    interface = NULL)
{
    tokens <- find_source_text_exprs(path, encoding)
    texts <- find_source_text_in_exprs(tokens, path = path, interface = interface)

    if (verbose) {
        cat(sep = "\n", sprintf(
            "Extracted %i source text(s) from '%s'.",
            length(texts),
            path))
    }

    return(texts)
}

#' @rdname find-source-text-in-exprs
#' @keywords internal
find_source_text_in_exprs <- function(
    tokens    = utils::getParseData(),
    path      = "",
    interface = NULL)
{
    # Parsing errors are skipped silently. This is required whenever
    # native pipes are used. They introduce placeholders (_) in expr
    # tokens, a special constant that makes no sense outside of the
    # full context. Some tokens are sub-exprs and lack the former,
    # which yields an error. tryCatch() introduces a non-negligible
    # overhead, but it is currently the only viable solution.
    code <- lapply(tokens$text, \(x) tryCatch(str2lang(x), error = \(c) NULL))

    is_src <- vapply_1l(code, is_source, interface = interface)

    locs <- map(source_location,
        more  = list(path = path),
        line1 = tokens[is_src, "line1"],
        col1  = tokens[is_src, "col1"],
        line2 = tokens[is_src, "line2"],
        col2  = tokens[is_src, "col2"])

    return(map(as_source_text, x = code[is_src], loc = locs))
}

#' @rdname find-source-text-in-exprs
#' @keywords internal
find_source_exprs <- function(path = "", encoding = "UTF-8") {
    # We use text_read() and parse(text = .) because
    # the former re-encodes source text to encoding.
    text   <- text_read(path, encoding)
    parsed <- parse(text = text, keep.source = TRUE, encoding = encoding)
    tokens <- utils::getParseData(parsed, TRUE)
    return(tokens[tokens$token == "expr", ])
}

#' @rdname find-source-text-in-exprs
#' @keywords internal
is_source <- function(x, interface = NULL) {
    # x must be a call and its firt element
    # must itself be a call to either :: or $.
    if (!is.call(x)) {
        return(FALSE)
    }

    x1 <- x[[1L]]

    # Check whether x1 is a `$`(<Translator>, translate) call.
    if (is.null(interface)) {
        return(
            is.call(x1) &&
            # to operator `$` and the latter is
            identical(as.name(x1[[1L]]), quote(`$`)) &&
            # fetching method/function translate and
            identical(as.name(x1[[3L]]), quote(translate)) &&
            # it is not passing ... to tr$translate().
            # This is a sign that x is being used to define
            # an interface. Such calls must be ignored.
            all(!vapply_1l(x[-1L], identical, y = quote(...))))
    }

    # Otherwise, check whether x1 is a custom
    # <interface>() or `::`(<ns>, <interface>) call.
    return(
        switch(class(interface),
            # call is to <interface> or
            name = identical(x1, interface),
            call = is.call(x1)  &&
                # call is to operator `::` and
                identical(x1[[1L]], quote(`::`)) &&
                # namespaces match and
                identical(as.name(x1[[2L]]), interface[[2L]]) &&
                # functions match.
                identical(as.name(x1[[3L]]), interface[[3L]]),
            FALSE))
}
