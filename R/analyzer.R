stream <- function(file = character(1L)) {
    validateFile(file)

    # Use R's built-in tokenizer.
    expr   <- parse(file, NULL, keep.source = TRUE)
    parsed <- getParseData(expr, NA)

    # Drop tokens that are tied to empty text.
    # This only slows down execution for what
    # we are trying to achieve.
    parsed <- parsed[nzchar(parsed$text), ]
    stream <- parsed$text

    # Attach tokens' locations to the stream.
    attr(stream, "line1") <- parsed$line1
    attr(stream, "col1")  <- parsed$col1
    attr(stream, "line2") <- parsed$line2
    attr(stream, "col2")  <- parsed$col2

    class(stream) <- c("TokenStream", "character")
    return(stream)
}

findCallNames <- function(stream = character(), name = character(1L)) {
    validateStream(stream)
    validateString(name)

    if (!length(stream)) {
        return(integer())
    }

    # A call is equivalent to an open parenthesis.
    # In a stream, calls are preceded by the names
    # of the functions being called.
    pos <- which(stream == "(") - 1L
    return(pos[stream[pos] == name])
}

findCallEnd <- function(stream = character(), start = integer(1L)) {
    validateStream(stream)
    validateIndex(start)

    streamLength <- length(stream)

    # Find next '(' token in the stream from start.
    while (stream[[start]] != "(") {
        start <- start + 1L

        # There could be no call at all in the stream.
        if (start > streamLength) {
            return(0L)
        }
    }

    # Track current call's depth. It increases
    # by 1 whenever an embedded call is detected.
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

# FIXME: here.
parseStream <- function(stream = character()) {
    validateStream(stream)
    return(str2lang(paste0(stream, collapse = "")))
}
