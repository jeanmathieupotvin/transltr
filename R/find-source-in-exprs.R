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
#' @template param-algorithm
#'
#' @template param-verbose
#'
#' @template param-interface
#'
#' @details
#' [find_source_in_exprs()] silently skips parsing errors. See [find_source()]
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
#' [find_source_in_file()] and [find_source_in_exprs()] return a list of
#' [`Text`][Text] objects. It may contain duplicated elements, depending
#' on the extracted contents.
#'
#' [find_source_exprs()] returns a subset of the output of
#' [utils::getParseData()]. Only `expr` tokens are returned.
#'
#' [is_source()] returns a logical value.
#'
#' @seealso
#' [`Text`][Text],
#' [find_source()],
#' [utils::getParseData()]
#'
#' @rdname find-source-in-file
#' @keywords internal
find_source_in_file <- function(
    path      = "",
    encoding  = "UTF-8",
    verbose   = getOption("transltr.verbose", TRUE),
    algorithm = algorithms(),
    interface = NULL)
{
    tokens <- find_source_exprs(path, encoding)
    texts  <- find_source_in_exprs(tokens,
        path      = path,
        algorithm = algorithm,
        interface = interface)

    if (verbose) {
        cat(sep = "\n", sprintf(
            "Extracted %i source text(s) from '%s'.",
            length(texts),
            path))
    }

    return(texts)
}

#' @rdname find-source-in-file
#' @keywords internal
find_source_in_exprs <- function(
    tokens    = utils::getParseData(),
    path      = "",
    algorithm = algorithms(),
    interface = NULL)
{
    # Parsing errors are skipped silently. This is required whenever
    # native pipes are used. They introduce placeholders (_) in expr
    # tokens, a special constant that makes no sense outside of the
    # full context. Some tokens are sub-exprs and lack the former,
    # which yields an error. tryCatch() introduces a non-negligible
    # overhead, but it is currently the only viable solution.
    code   <- lapply(tokens$text, \(x) tryCatch(str2lang(x), error = \(c) NULL))
    is_src <- vapply_1l(code, is_source, interface = interface)

    locs <- map(location, more = list(path = path),
        line1 = tokens[is_src, "line1"],
        col1  = tokens[is_src, "col1"],
        line2 = tokens[is_src, "line2"],
        col2  = tokens[is_src, "col2"])

    return(
        map(as_text,
            x    = code[is_src],
            loc  = locs,
            more = list(algorithm = algorithm)))
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

#' @rdname find-source-in-file
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
