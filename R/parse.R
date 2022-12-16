validateFile <- function(file = character(1L)) {
    if (!is.character(file) || !length(file) || !nzchar(file) || is.na(file)) {
        stopf("TypeError", "`file` must be a non-empty, non-NA character string.")
    }
    if (!utils::file_test("-f", file)) {
        stopf("InterfaceError", "[%s] is not a valid `file`.", file)
    }

    return(invisible(file))
}

validateTokens <- function(tokens) {
    # tokens must be a data.frame created
    # by function utils::getParseData().
    if (is.null(tokens$token)) {
        stopf("LogicError", "`tokens` must have a column named 'token'.")
    }
    if (is.null(tokens$text)) {
        stopf("LogicError", "`tokens` must have a column named 'text'.")
    }

    return(invisible(tokens))
}

validateSymbol <- function(sym = character(1L)) {
    if (!is.character(sym) || !length(sym) || !nzchar(sym) || is.na(sym)) {
        stopf("TypeError", "`sym` must be a non-empty, non-NA character string.")
    }

    return(invisible(sym))
}

tokenize <- function(file = character(1L)) {
    validateFile(file)

    expr   <- parse(file, NULL, keep.source = TRUE)
    tokens <- getParseData(expr, NA)

    # Comments only slows down execution later.
    return(tokens[tokens$token != "COMMENT", ])
}

extractSymbolFunctionCalls <- function(tokens, sym) {
    validateTokens(tokens)
    validateSymbol(sym)

    # Extract srcfile object from tokens
    # to attach it to extracted calls.
    srcFile <- attr(tokens, "srcfile")

    # Local function to extract one call.
    extractSymFunCall <- function(i = integer(1L)) {
        # Symbol function calls tokens are
        # always followed by a '(' token.
        j  <- i + 1L
        id <- tokens$parent[[j]]

        # Find the matching ')' token. It has the
        # same parent ID as the '(' token above.
        while (ctokens[[j]] != "')'" || cparents[[j]] != id) {
            j <- j + 1L
        }

        # Indices i and j marks the beginning and the
        # end of a typical function call: sym(...). All
        # tokens between i and j (inclusively) are part
        # of the call to extract.
        callIndices  <- seq.int(i, j, 1L)
        callLocation <- c(
            tokens[i, c("line1", "col1"), drop = TRUE],
            tokens[j, c("line2", "col2"), drop = TRUE])

        return(
            structure(
                str2lang(collapse(ctexts[callIndices])),
                srcFile  = srcFile,
                index    = i,
                indices  = callIndices,
                location = callLocation))
    }

    # Extract columns from tokens for faster extractions.
    ctokens  <- tokens$token
    cparents <- tokens$parent
    ctexts   <- tokens$text

    # Get positions of calls to function sym in tokens.
    # This also validates arguments tokens and sym.
    symFunCallsIndices <- which(
        ctokens == "SYMBOL_FUNCTION_CALL" &
        ctexts  == sym)

    return(lapply(symFunCallsIndices, extractSymFunCall))
}

extractArgFromCalls <- function(
    calls = list(),
    sym   = character(1L),
    pos   = integer(1L))
{
    validateSymbol(sym)

    if (!is.integer(pos) || !length(pos) || is.na(pos) || pos <= 0L) {
        stopf("TypeError", "`pos` must be a non-NA, positive integer value.")
    }

    # Local function to extract sym from one call.
    extractArgFromCall <- function(callObj = call()) {
        if (!inherits(callObj, "call")) {
            stopf("TypeError", "`callObj` must be a `call` object.")
        }

        # First element is a function name.
        # Further elements are the values
        # passed to its arguments. Values
        # are named whenever arguments are
        # explicitly stated in the call.
        callList <- as.list(callObj)

        # Extract by name if possible.
        if (!is.null(value <- callList[[sym]])) {
            return(value)
        }

        # Else, extract by position.
        return(callList[[pos + 1L]])
    }

    return(lapply(calls, extractArgFromCall))
}

toTranslate <- function(file = character(1L)) {
    tokens  <- tokenize(file)
    calls   <- extractSymbolFunctionCalls(tokens, .FUN_NAME)
    values  <- extractArgFromCalls(calls, .ARG_NAME, .ARG_POS)
    strings <- unlist(values)


    # FIXME: strings must be checked by isTagged().
    # FIXME: strings must convey the information passed to attributes of calls.
    # FIXME: this function name must be renamed accordingly.
    tokens[match(strings, gsub("\"", "", tokens$text)), ]
    strings <- untag(strings[isTagged(strings)])

    return()
}


# Internal constants -----------------------------------------------------------


# Parameters controlling values extracted by toTranslate().
.FUN_NAME <- "translate"
.ARG_NAME <- "text"
.ARG_POS  <- 1L
