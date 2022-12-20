#' Extract text from source code
#'
#' Statically [analyze()] \R scripts (without executing the source code) and
#' extract character strings to be translated.
#'
#' @param file A file path. It must point to an existing \R script.
#'
#' @details
#' The internal engine relies on [base::parse()] to decompose \R code into
#' sequences of *tokens* (single elements of the language). For more information
#' on \R tokens, see [utils::getParseData()].
#'
#' @note
#' What follows describes the steps that [analyze()] goes through each time it
#' is called. **Users typically do not need to bother with these details.**
#'
#'   1. Contents of `file` is first transformed into a sequence of tokens by
#'      [tokenize()]. Such a sequence is internally called a *stream*. Empty
#'      strings representing `expr` tokens are conveniently removed.
#'
#'   2. Calls (`'('` tokens) made to [translate()] in the `stream` are located
#'      by [findCalls()]. This function extracts indices of the underlying
#'      `SYMBOL_FUNCTION_CALL` tokens.
#'
#'   3. For each index returned by [findCalls()], the `stream` is traversed by
#'      [findCallEnd()] to locate where each call ends. It does so by attempting
#'      to detect a matching outer `')'` token.
#'
#'   4. The stream is split into many sub-streams according to what [findCalls()]
#'      and [findCallEnd()] returned. Each sub-stream is parsed as an unevaluated
#'      [call][base::call()] object by [parseStream()].
#'
#'   5. The actual value passed to argument `text` is extracted from each call
#'      by function [extractCallArgumentValue()].
#'
#'   6. The extracted values are checked. Since [analyze()] performs a
#'      static code analysis, these must be litteral character strings.
#'
#' With further work, this limitation could be lifted (for some cases) in the
#' future.
#'
#' @returns
#' A named list of length 6 containing the following elements:
#'
#' \describe{
#'   \item{`file`}{Path to the underlying \R script. See `file` above.}
#'   \item{`workingDir`}{Current working directory.}
#'   \item{`encoding`}{Encoding of `file`.}
#'   \item{`timeStamp`}{Last time `file` was modified in Coordinated Universal
#'     Time (UTC).}
#'   \item{`nCalls`}{Number of detected calls to [translate()].}
#'   \item{`text`}{Each element in `text` is a named list of length 2
#'     containing the following elements:
#'
#'   \describe{
#'     \item{`location`}{The `location` of a call to [translate()] in file.
#'       The format is `"Ln X, Col Y"`, where `X` and `Y` are both positive
#'       integers.}
#'     \item{`value`}{The actual value passed to formal argument `text` of
#'       [translate()] either by name (preferred) or by position.}
#'   }}
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


# Low-level engine -------------------------------------------------------------


# FIXME: document me in analyze-engine.Rd.
# FIXME: mark me as an internal function.
tokenize <- function(file = character(1L)) {
    assertString(file)

    if (!utils::file_test("-f", file)) {
        stopf("InterfaceError", "`file` must be an existing file.")
    }

    # Use R's built-in tokenizer.
    expr    <- parse(file, NULL, keep.source = TRUE)
    parsed  <- utils::getParseData(expr, NA)
    srcfile <- attr(parsed, "srcfile")

    # Keep only tokens that are not empty strings.
    stream  <- parsed$text
    keep    <- nzchar(stream)
    srclocs <- sprintf("Ln %i, Col %i", parsed$line1[keep], parsed$col1[keep])

    return(
        structure(
            stream[keep],
            workingDir = srcfile$wd,
            file       = srcfile$filename,
            encoding   = srcfile$Enc,
            timeStamp  = format(srcfile$timestamp, tz = "UTC", usetz = TRUE),
            locations  = srclocs))
}

# FIXME: document me in analyze-engine.Rd.
# FIXME: mark me as an internal function.
findCalls <- function(stream = character(), name = "translate") {
    assertNonEmptyCharacter(stream)
    assertNoEmptyStrings(stream)
    assertString(name)

    # Calls are identified by a '(' token. In
    # a stream of tokens, they are preceded by
    # by the name of the function being called.
    pos <- which(stream == "(")
    return(pos[stream[pos - 1L] == name])
}

# FIXME: document me in analyze-engine.Rd.
# FIXME: mark me as an internal function.
findCallEnd <- function(stream = character(), start = integer(1L)) {
    assertNonEmptyCharacter(stream)
    assertNoEmptyStrings(stream)
    assertScalarInteger(start)

    nTokens <- length(stream)

    if (start <= 0L || start >= nTokens) {
        stopf(
            "LogicError",
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

# FIXME: document me in analyze-engine.Rd.
# FIXME: mark me as an internal function.
parseStream <- function(stream = character()) {
    assertNonEmptyCharacter(stream)
    assertNoEmptyStrings(stream)
    return(str2lang(paste0(stream, collapse = "")))
}

# FIXME: document me in analyze-engine.Rd.
# FIXME: mark me as an internal function.
extractCallArgumentValue <- function(call, argName = "text", argPos  = 1L) {
    assertString(argName)
    assertScalarInteger(argPos)

    if (!is.call(call)) {
        stopf("TypeError", "`call` must be a `call` object.")
    }

    callList <- as.list(call)

    # Extract value by argument's name if possible.
    if (!is.null(value <- callList[[argName]])) {
        return(value)
    }

    # Else, extract value by position. argPos
    # is shifted by 1 because the function's
    # name is inserted into the list.
    return(callList[[argPos + 1L]])
}
