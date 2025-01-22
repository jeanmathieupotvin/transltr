#' Find Source Text in Expression Tokens
#'
#' @description
#' Find, and extract source text that requires translation from a single file,
#' or from a set of tokenized \R expressions stemming from [parse()].
#'
#' **Some arguments listed below are not explicitly validated for efficiency.**
#'
#' @param path A non-empty and non-[NA][base::NA] character string. A path to
#'   an \R source script.
#'
#' @param tokens A [`data.frame`][data.frame()] returned by
#'   [utils::getParseData()]. It must always minimally contain
#'   columns `line1`, `col1`, `line2`, `col2`, and `text`.
#'
#' @template param-encoding
#'
#' @template param-strict
#'
#' @template param-algorithm
#'
#' @template param-verbose
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
#' [`Text`][Text],
#' [translate()],
#' [find_source()]
#'
#' @rdname find-source-in-file
#' @keywords internal
find_source_in_file <- function(
    path      = "",
    encoding  = "UTF-8",
    strict    = TRUE,
    algorithm = constant("algorithms"),
    verbose   = FALSE)
{
    tokens <- find_source_exprs(path, encoding)
    texts  <- find_source_in_exprs(tokens, path, strict, algorithm)

    if (verbose) {
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
    tokens    = utils::getParseData(),
    path      = "",
    strict    = TRUE,
    algorithm = constant("algorithms"))
{
    # Parsing errors are skipped silently. This is required whenever
    # native pipes are used. They introduce placeholders (_) in expr
    # tokens, a special constant that makes no sense outside of the
    # full context. Some tokens are sub-exprs and lack the former,
    # which yields an error. tryCatch() introduces a non-negligible
    # overhead, but it is the only viable solution.
    code      <- lapply(tokens$text, \(x) tryCatch(str2lang(x), error = \(c) NULL))
    is_call   <- vapply_1l(code, is_translate_call, strict = strict)
    locations <- map(location, more = list(path = path),
        line1 = tokens[is_call, "line1"],
        col1  = tokens[is_call, "col1"],
        line2 = tokens[is_call, "line2"],
        col2  = tokens[is_call, "col2"])

    return(
        map(as_text,
            x        = code[is_call],
            location = locations,
            more     = list(
                strict    = strict,
                algorithm = algorithm,
                validate  = FALSE)))
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
