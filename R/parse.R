tokenizeScript <- function(file = character(1L)) {
    if (!is.character(file) || !length(file) || !nzchar(file)) {
        stopf("TypeError", "`file` must be a non-empty character string.")
    }

    if (!utils::file_test("-f", file)) {
        stopf("InterfaceError", "[%s] is not a valid `file`.", file)
    }

    tokens <- getParseData(parse(file), NA)
    class(tokens) <- c("TokensTable", class(tokens))
    return(tokens)
}

isTokensTable <- function(x) {
    return(inherits(x, "TokensTable"))
}

getFunctionCallsIndices <- function(
    tokens = data.frame(),
    fun    = character(1L))
{
    if (!isTokensTable(tokens)) {
        stopf("TypeError", "`tokens` must be a `TokensTable` object.")
    }
    if (!is.character(fun) || length(fun) != 1L || !nzchar(fun)) {
        stopf("TypeError", "`fun` must be a non-empty character string.")
    }

    return(which(tokens$token == "SYMBOL_FUNCTION_CALL" & tokens$text  == fun))
}

getTextToTranslate <- function(file = character(1L)) {
    # String to translate is passed to argument
    # ARG_NAME of function FUN_NAME in package.
    FUN_NAME <- "translate"
    ARG_NAME <- "text"

    tokens <- tokenizeScript(file)
    translateCallsIndices <- getFunctionCallsIndices(tokens, FUN_NAME)

    # Extract values passed to argument
    # text of function transltr::translate().
    tokensToTranslate <- lapply(translateCallsIndices, \(i) {

        # Case 1 : value of argument text is passed by position.
        # Example: translate("{{ ... }}").
        #
        # Value will be preceded by token "(", then by an implicit "expr"
        # token, and then by a "SYMBOL_FUNCTION_CALL" token.
        #
        # The value to extract is a STR_CONST token.
        if (tokens[ip3 <- i + 3L, "token"] == "STR_CONST") {

            # To avoid extracting values passed to another translate()
            # function, we check that the extracted "STR_CONST" token
            # is enclosed by double brackets.
            if (!isTagged(tokens[ip3, "text"])) {
                return(NULL)
            }

            return(tokens[ip3, ])
        }

        # Case 2 : value of argument is passed explicitly.
        # Example: translate(text = "{{ ... }}").
        #
        # Value may not be passed first and may be preceded by many tokens.
        # This value may appear anywhere between the "(" and ")" tokens of
        # the underlying "expr" token.
        j      <- i
        exprId <- tokens[i + 1L, "id"]

        # Traverse elements of the underlying "expr"
        # token until its closing ")" token is detected.
        while (tokens[j, "token"] != ")" || tokens[j, "parent"] != exprId) {

            # Stop if a "SYMBOL_SUB" token matching argument "text" is
            # encountered. It is always followed by an "EQ_SUB" token.
            #
            # The value to extract is the "STR_CONST" token that follows.
            if (tokens[j, "token"] == "SYMBOL_SUB" &&
                tokens[j, "text"]  == ARG_NAME) {

                # To avoid extracting values passed to another translate()
                # function, we check that the extracted "STR_CONST" token
                # is enclosed by double brackets.
                if (!isTagged(tokens[j + 2L, "text"])) {
                    return(NULL)
                }

                return(tokens[j + 2L, ])
            }

            j <- j + 1L
        }
    })

    # rbind() drops NULL elements and passes class(es)
    # of tokens to the concatenated output because all
    # remaining elements arerows of tokens.
    return(do.call(rbind, tokensToTranslate))
}
