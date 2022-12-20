TokenStream <- function(file = character(1L)) {
    assertString(file)

    if (!utils::file_test("-f", file)) {
        stopf("InterfaceError", "`file` must be an existing file.")
    }

    # Use R's built-in tokenizer.
    expr   <- parse(file, NULL, keep.source = TRUE)
    parsed <- getParseData(expr, NA)
    stream <- parsed$text

    # Attach tokens' locations to the stream.
    return(
        structure(stream,
            srcloc = sprintf("%i:%i", parsed$line1, parsed$col1),
            class  = c("TokenStream", "character")))
}

#' @export
`[.TokenStream` <- function(x, i) {
    return(
       structure(NextMethod("["),
           srcloc = attr(x, "srcloc")[i],
           class  = c("TokenStream", "character")))
}

findCalls <- function(stream = character(), name = character(1L)) {
    assertNonEmptyCharacter(stream)
    assertString(name)

    # A call is identified by a '(' token. In
    # a TokenStream, calls are preceded by an
    # empty string and then by the name of the
    # function being called.
    pos <- which(stream == "(")
    return(pos[stream[pos - 2L] == name])
}

findCallEnd <- function(stream = character(), start = integer(1L)) {
    assertNonEmptyCharacter(stream)
    assertScalarInteger(start)

    nTokens <- length(stream)

    if (start <= 0L || start >= nTokens) {
        stopf(
            "LogicError",
            "`start` must be greater than 0 and less than or equal to `length(stream)`.")
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

    while (TRUE) {
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
}

createSequences <- function(from = integer(), to = integer()) {
    assertNonEmptyInteger(from)
    assertNonEmptyInteger(to)

    if (length(from) != length(to)) {
        stopf("LogicError", "`from` and `to` must have equal lengths.")
    }

    return(.mapply(seq.int, list(from = from, to = to), list(by = 1L)))
}

parseStream <- function(stream = character()) {
    assertNonEmptyCharacter(stream)
    return(str2lang(paste0(stream, collapse = "")))
}
