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
    starts    <- findCalls(stream, "translate") - 1L
    ends      <- vapply1i(starts, findCallEnd, stream = stream)
    locations <- attr(stream, "locations")[starts]

    # Each pair of start and end values defines
    # a set of tokens to be parsed as a call.
    streams <- map(\(s, e) stream[seq.int(s, e, 1L)], s = starts, e = ends)
    calls   <- lapply(streams, parseStream)

    # Extract value passed to argument
    # text from calls to translate().
    values   <- lapply(calls, extractCallArgumentValue, "text", 1L)
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

findCalls <- function(stream = character(), name = character(1L)) {
    assertNonEmptyCharacter(stream)
    assertString(name)

    # Calls are identified by a '(' token. In
    # a stream of tokens, they are preceded by
    # by the name of the function being called.
    pos <- which(stream == "(")
    return(pos[stream[pos - 1L] == name])
}

findCallEnd <- function(stream = character(), start = integer(1L)) {
    assertNonEmptyCharacter(stream)
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

parseStream <- function(stream = character()) {
    assertNonEmptyCharacter(stream)
    return(str2lang(paste0(stream, collapse = "")))
}

extractCallArgumentValue <- function(
    call    = call(),
    argName = character(1L),
    argPos  = integer(1L))
{
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
