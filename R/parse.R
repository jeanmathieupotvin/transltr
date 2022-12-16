TO_BE_NAMED <- function(file = character(1L)) {
    tokens <- tokenize(file)
    tags   <- getTransltrComments(tokens)

    # Parse translate() calls and extract values
    # passed to its (first) formal argument text.
    calls  <- getSymbolFunctionCalls(tokens, "translate")
    values <- unlist(getArgFromCalls(calls, "text", 1L))

    # Only keep values marked for translation.
    keep <- isTagged(values)

    # Construct SrcText objects from kept values/calls.
    return(
        mapply(newSrcText,
            text     = untag(strings[keep]),
            lang     = tags$sourceLanguage,
            file     = file,
            location = lapply(calls[keep], attr, which = "srcLocation"),
            SIMPLIFY = FALSE))
}


# Parser/engine ----------------------------------------------------------------


tokenize <- function(file = character(1L)) {
    validateFile(file)
    return(getParseData(parse(file, NULL, keep.source = TRUE), NA))
}

getTransltrComments <- function(tokens = data.frame()) {
    validateTokens(tokens)

    colToken <- tokens$token
    colText  <- tokens$text

    # Extract COMMENT tokens that
    # are specific to the package.
    comments <- colText[
        colToken == "COMMENT" &
        startsWith(colText, .TRANSLTR_COMMENT)]

    # Each comment becomes a vector of 4 elements:
    #   - the first two are useless,
    #   - the third one is the tag's name, and
    #   - the last one is its value.
    #
    # A regular expression is used to split
    # the unparsed comment into 4 elements:
    # @      : matches @ literally;
    # |      : OR;
    # [ \t]* : matches space(s) and tab(s) ([ \t]) zero or multiple times (*).
    splits <- strsplit(comments, "@|[ \t]+")
    values <- lapply(splits, `[[`, i = 4L)

    names(values) <- vapply(splits, `[[`, NA_character_, i = 3L)
    return(values)
}

getSymbolFunctionCalls <- function(
    tokens = data.frame(),
    symbol = character(1L))
{
    validateTokens(tokens)
    validateString(symbol)

    # Local function to extract one call.
    getOne <- function(i = integer(1L)) {
        # Symbol function calls tokens are
        # always followed by a '(' token.
        j  <- i + 1L
        id <- colParent[[j]]

        # Find the matching ')' token. It has the
        # same parent ID as the '(' token above.
        while (colToken[[j]] != "')'" || colParent[[j]] != id) {
            j <- j + 1L
        }

        # i and j mark the beginning and the end of a
        # typical function call: symbol(...). Tokens
        # between i and j (inclusively) are part of
        # the call.
        return(
            structure(
                str2lang(collapse(colText[seq.int(i, j, 1L)])),
                srcFile     = srcFile,
                srcLocation = newSrcLocation(tokens, i, j)))
    }

    # Extract srcfile object from tokens
    # to attach it to extracted calls.
    srcFile <- attr(tokens, "srcfile")

    # Extract columns from tokens for faster extractions.
    colToken  <- tokens$token
    colParent <- tokens$parent
    colText   <- tokens$text

    # Extract positions of SYMBOL_FUNCTION_CALL tokens.
    # They mark the beginning of unparsed function calls.
    symFunCalls <- which(
        colToken == "SYMBOL_FUNCTION_CALL" &
        colText  == symbol)

    return(lapply(symFunCalls, getOne))
}

getArgFromCalls <- function(
    calls  = list(),
    symbol = character(1L),
    pos    = integer(1L))
{
    if (!is.list(calls) || any(classes(calls) != "call")) {
        stopf("TypeError", "`calls` must be a list of `call` object.")
    }

    validateString(symbol)
    validateIndex(pos)

    # Local function to extract value from one call.
    getOne <- function(callObj = call()) {
        callList <- as.list(callObj)

        # Extract by name if possible.
        if (!is.null(value <- callList[[symbol]])) {
            return(value)
        }

        # Else, extract by position.
        return(callList[[pos + 1L]])
    }

    return(lapply(calls, getOne))
}
