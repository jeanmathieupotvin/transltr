#' Extract text from source code
#'
#' Statically [analyze()] \R scripts (without executing the source code) and
#' extract character strings to be translated.
#'
#' The internal engine relies on [base::parse()] to decompose \R code into
#' sequences of *tokens* (single elements of the language). For more information
#' on \R tokens, see [utils::getParseData()].
#'
#' @param file A file path. It must point to an existing \R script.
#'
#' @note
#' What follows describes the steps that [analyze()] goes through each time it
#' is called. **Users typically do not need to bother with these details.**
#'
#' 1. Contents of `file` is first transformed into a sequence of tokens by
#'    [tokenize()]. Such a sequence is internally called a *stream*. Empty
#'    strings representing `expr` tokens are conveniently removed.
#'
#' 2. Calls (`'('` tokens) made to [translate()] in the `stream` are located
#'    by [findCalls()]. This function extracts indices of the underlying
#'    `SYMBOL_FUNCTION_CALL` tokens.
#'
#' 3. For each index returned by [findCalls()], the `stream` is traversed by
#'    [findCallEnd()] to locate where each call ends. It does so by attempting
#'    to detect a matching outer `')'` token.
#'
#' 4. The stream is split into many sub-streams according to what [findCalls()]
#'    and [findCallEnd()] returned. Each sub-stream is parsed as an unevaluated
#'    [call][base::call()] object by [parseStream()].
#'
#' 5. The actual value passed to argument `text` is extracted from each call
#'    by function [extractCallArgumentValue()].
#'
#' 6. The extracted values are checked.
#'
#' Since [analyze()] performs a static code analysis, extracted values must be
#' litteral character strings. With further work, this limitation could be
#' lifted (for some cases) in the future.
#'
#' @returns
#' A named list of length 6 containing the following elements:
#'
#' \item{`file`}{
#'   Path to the underlying \R script. See `file` above.
#' }
#' \item{`workingDir`}{
#'   Current working directory.
#' }
#' \item{`encoding`}{
#'   Encoding of `file`.
#' }
#' \item{`timeStamp`}{
#'   Last time `file` was modified in Coordinated Universal Time (UTC).
#' }
#' \item{`nCalls`}{
#'   Number of detected calls to [translate()].
#' }
#' \item{`text`}{
#'   A list. See below.
#' }
#'
#' Each element of `text` is itself a named list of length 2 containing the
#' following elements:
#'
#' \item{`location`}{
#'   The `location` of a call to [translate()] in `file`.
#'   The format is `"Ln X, Col Y"`, where `X` and `Y` are positive integers.
#' }
#' \item{`value`}{
#'   The actual value passed to formal argument `text` of
#'   [translate()] either by name (preferred) or by position.
#' }
#'
#' @author Jean-Mathieu Potvin (<jm@@potvin.xyz>)
#'
#' @keywords internal
analyze <- function(file = character(1L)) {
    stream <- tokenize(file)
    meta   <- attributes(stream)[c(
        "file",
        "workingDir",
        "encoding",
        "timeStamp")]

    # findCalls() returns the positions of the
    # matching '(' tokens. The function's name
    # that precedes it must be included.
    starts    <- findCalls(stream) - 1L
    ends      <- vapply1i(starts, findCallEnd, stream = stream)
    locations <- attr(stream, "locations")[starts]

    # Each pair of start and end values defines
    # a set of tokens to be parsed as a call.
    streams <- map(\(s, e) stream[seq.int(s, e, 1L)], s = starts, e = ends)
    calls   <- lapply(streams, parseStream)

    # Extract value passed to argument
    # text from calls to translate().
    values   <- lapply(calls, extractCallArgumentValue)
    isNotChr <- vapply1c(values, class) != "character"

    if (any(isNotChr)) {
        stopf(
            "InterfaceError",
            "%s: value passed to argument `text` of `transltr::translate()` must be a litteral character string.",
            locations[isNotChr][[1L]])
    }

    text <- map(list, location = locations, value = values)
    return(c(meta, nCalls = length(calls), list(text = text)))
}


# Engine's back-end ------------------------------------------------------------


#' Backend of the static analyzer
#'
#' These functions are internally used by function [analyze()]. They should
#' not be used elsewhere.
#'
#' The engine has a single entry point: [analyze()]. It assembles individual
#' parts described here and mainly goes through the steps described in section
#' *Note* of the latter.
#'
#' \describe{
#'
#' \item{`tokenize()`}{
#'   This is basically a convenient wrapper function to [base::parse()] and
#'   [utils::getParseData()], but handles big character strings differently.
#'   It uses the tokens and their locations returned by \R's built in parser.
#' }
#' \item{`findBigStrings()`}{
#'   It traverses a `stream` once to identify so-called *big strings*:
#'   litteral character strings tokens that are truncated and replaced
#'   by a short description:
#'
#'   ```r
#'   "[4019 chars quoted with '\"']"  ## or
#'   "[4019 chars quoted with ''']"
#'   ```
#'
#'   The final token depends on its length and on the symbol used to declare
#'   a litteral character string.
#' }
#' \item{`findCalls()`}{
#'   It traverses a `stream` once to identify all calls to `name`. It looks
#'   for `"("` tokens and extracts the `SYMBOL_FUNCTION_CALL` token that
#'   immediately precedes it.
#'
#'   It currently does **not** support function called by [base::call()]
#'   itself or by primitive function [`base::(`][base::Paren]. This
#'   limitation may be lifted in the future.
#' }
#' \item{`findCallEnd()`}{
#'   It traverses a `stream` once starting at `start` to find a `")"` token
#'   matching the `"("` token found by [findCalls()]. It stops as soon as the
#'   latter is detected to minimize the number of required iterations.
#'
#'   Argument `start` is arbitrary and does not need to be the index of a
#'   `"("` token. In that case, `findCallEnd()` loops through the elements
#'   of `stream` until it encounters a `"("` and returns the position of the
#'   matching `")"` token.
#'
#'   If it reaches the end of `stream` before detecting a corresponding `")"`
#'   token (or a `"("` token if `start` does not point to such a token), it
#'   returns `0L`. This is an error signal.
#' }
#' \item{`parseStream()`}{
#'   This function is a convenient wrapper to [base::paste0()] and
#'   [base::str2lang()].
#' }
#' \item{`extractCallArgumentValue()`}{
#'   It attempts to extract `argName` from `cl`, a [call][base::call()]
#'   object. If it is not specified, `argPos` is extracted instead. An error
#'   is thrown if both are not possible.
#' }
#' }
#'
#' @name analyze-backend
#'
#' @param stream A *stream* of non-empty tokens created by [tokenize()].
#'   See Details.
#'
#' @param name A function's name.
#'
#' @param start An index stating where to start in `stream` when *traversing*
#'   it (when looping through its elements).
#'
#' @param cl A [call][base::call()].
#'
#' @param argName Expected name of a function's argument.
#'
#' @param argPos Expected position of a function's argument (in the underlying
#'   [call][base::call()]).
#'
#' @inheritParams analyze
#'
#' @returns
#' [tokenizer()] returns a character vector of non-empty, non-[NA][base::NA]
#' values. It attaches the following attributes to it:
#'
#' \item{`file`}{
#'   Path to the underlying \R script. See `file` above.
#' }
#' \item{`workingDir`}{
#'   Current working directory.
#' }
#' \item{`encoding`}{
#'   Encoding of `file`.
#' }
#' \item{`timeStamp`}{
#'   Last time `file` was modified in Coordinated Universal Time (UTC).
#' }
#' \item{`locations`}{
#'   The `locations` of calls made to `name` in `file`. The format is
#'   `"Ln X, Col Y"`, where `X` and `Y` are positive integers.
#' }
#'
#' All these attributes are later passed down to the output of [analyze()].
#' They stem from
#'
#' 1. the underlying [srcfile][base::srcfile()] object returned by
#'    [base::parse()], and
#'
#' 2. from further attached information when `keep.source` is `TRUE`.
#'
#' [findBigStrings()] and [findCalls()] return an integer vector.
#'
#' [findCallEnd()] returns a single integer value. It returns `0` if a call
#' has no end (no corresponding `")"` token). This value must be treated as
#' an error and is a sign that `stream` is not semantically valid according
#' to the \R language.
#'
#' [parseStream()] returns a [language][base::is.language()] object. This
#' will typically be a [call][base::call()] but could also be something *else*
#' (an [ȩxpression][base::expression()], a [symbol][base::is.symbol()], etc.)
#' depending on the underlying `stream`.
#'
#' [extractCallArgumentValue()] may return *almost* anything, depending on the
#' context: a [language][base::is.language()] object, an
#' [atomic][base::vector()] vector, etc. It is expected that it returns a
#' single character value when `name`, `argName`, and `argPos` are respectively
#' equal to `"translate"`, `"text"`, and `1L`.
#'
#' @author Jean-Mathieu Potvin (<jm@@potvin.xyz>)
#'
#' @keywords internal
tokenize <- function(file = character(1L)) {
    assertString(file)

    if (!utils::file_test("-f", file)) {
        stopf("InterfaceError", "`file` must be an existing file.")
    }

    # Use R's built-in tokenizer.
    expr    <- parse(file, NULL, keep.source = TRUE)
    parsed  <- utils::getParseData(expr, NA)
    srcfile <- attr(parsed, "srcfile")
    stream  <- parsed$text

    # Big strings are manually injected into stream.
    # utils::getParseData(, TRUE) yields a different
    # AST and is avoided because it makes no sense
    # at all.
    if (length(bigStrings <- findBigStrings(stream))) {
       bigStringsIds      <- parsed$id[bigStrings]
       stream[bigStrings] <- getParseText(parsed, bigStringsIds)
    }

    # Keep only tokens that are not empty strings.
    keep <- nzchar(stream)

    return(
        structure(
            stream[keep],
            file       = srcfile$filename,
            workingDir = srcfile$wd,
            encoding   = srcfile$Enc,
            timeStamp  = format(srcfile$timestamp, tz = "UTC", usetz = TRUE),
            locations  = sprintf("Ln %i, Col %i",
                parsed$line1[keep],
                parsed$col1[keep])))
}

#' @rdname analyze-backend
#' @keywords internal
findBigStrings <- function(stream = character()) {
    # ^      : matches the beginning of a string;
    # \\[    : matches a single [ character literally;
    # [0-9]* : matches any digit zero or multiple times (*);
    # ---
    # '\"'   : matches string '"' once literally
    # |      : OR
    # '''    : matches string ''' literally;
    # ---
    # \\]    : matches a single ] character literally;
    # $      : matches the end of a string;
    return(grep("^\\[[0-9]* chars quoted with '\"'|'''\\]$", stream))
}

#' @rdname analyze-backend
#' @keywords internal
findCalls <- function(stream = character(), name = "translate") {
    # Calls are identified by a '(' token. In
    # a stream of tokens, they are preceded by
    # by the name of the function being called.
    pos <- which(stream == "(")
    return(pos[stream[pos - 1L] == name])
}

#' @rdname analyze-backend
#' @keywords internal
findCallEnd <- function(stream = character(), start = integer(1L)) {
    nTokens <- length(stream)

    if (start <= 0L || start >= nTokens) {
        stopf(
            "TypeError",
            "`start` must be greater than 0 and less than `stream`'s length'.")
    }

    # Find next '(' token in the stream
    # starting at position given by start.
    while (stream[[start]] != "(") {
        start <- start + 1L

        # There could be no call in the stream.
        if (start > nTokens) {
            return(0L)
        }
    }

    # Track current call's depth.
    # It is increased by 1 whenever an embedded '(' is
    # detected and decreased by 1 whenever an embedded
    # ')' ends.
    depth <- 0L
    end   <- start

    while (end <= nTokens) {
        # Adjust depth based on current token.
        depth <- depth + switch(stream[[end]],
            "(" =  1L,
            ")" = -1L,
            0L)

        # A depth of 0 implies that a matching outer
        # ')' token was detected. Its position is j.
        if (depth == 0L) {
            return(end)
        }

        end <- end + 1L
    }

    # end should never be greater than the length
    # of stream because it is created via parse().
    # 0 is returned if no matching outer ')' is
    # detected.
    return(0L)
}

#' @rdname analyze-backend
#' @keywords internal
parseStream <- function(stream = character()) {
    return(str2lang(paste0(stream, collapse = "")))
}

#' @rdname analyze-backend
#' @keywords internal
extractCallArgumentValue <- function(
    cl      = call(),
    argName = "text",
    argPos  = 1L)
{
    if (!is.call(cl)) {
        stopf("TypeError", "`cl` must be a `call` object.")
    }

    # Extract value by argument's name if possible.
    if (!is.null(value <- cl[[argName]])) {
        return(value)
    }

    # Else, extract value by position. argPos
    # is shifted by 1 because the function's
    # name is part of the call.
    if ({ i <- argPos + 1L } > length(cl)) {
        stopf(
            "LogicError",
            "`%s`: `argPos` is greater than the number of supplied arguments.",
            strtrim(deparse(cl), 40L))
    }

    return(cl[[argPos + 1L]])
}
