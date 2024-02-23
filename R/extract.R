extract <- function(..., fsep = c("/", "\\")) {
    assertChoice(fsep) # slightly more optimal if called here
    return(lapply(c(...), extract1, fsep = fsep))
}

extract1 <- function(path = character(1L), fsep = c("/", "\\")) {
    file   <- File(path, fsep)
    tokens <- utils::getParseData(parse(path, keep.source = TRUE), TRUE)
    exprs  <- tokens[tokens$token == "expr", ]
    slocs  <- map(SrcLoc, moreArgs = list(file = file),
        line1 = exprs$line1,
        col1  = exprs$col1,
        line2 = exprs$line2,
        col2  = exprs$col2)

    # Convert raw expressions into SrcString objects.
    # Only relevant calls are kept before doing so.
    # The chain of coercions performed is as follows:
    # token -> SrcExpr -> SrcTranslation -> SrcString.
    sexprs  <- map(SrcExpr, str = exprs$text, sloc = slocs)
    strings <- sexprs[vapply1l(sexprs, isTranslateCall)] |>
        lapply(asSrcTranslation) |>
        lapply(asSrcString)

    attr(strings, "file") <- file
    names(strings) <- vapply1c(strings, `[[`, i = "hash")
    return(strings)
}
