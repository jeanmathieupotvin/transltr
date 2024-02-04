extract <- function(path = character(1L)) {
    if (!isNonEmptyString(path)) {
        halt("'path' must be a non-NA and non-empty character of length 1.")
    }
    if (!utils::file_test("-f", path <- normalizePath(path, "/", FALSE))) {
        halt("'%s' either does not exist or is not a valid file.", path)
    }

    tokens <- utils::getParseData(parse(path, keep.source = TRUE), TRUE)
    exprs  <- tokens[tokens$token == "expr", ]

    slocs <- map(SrcLoc, moreArgs = list(file = path),
        line1 = exprs$line1,
        col1  = exprs$col1,
        line2 = exprs$line2,
        col2  = exprs$col2)

    sexprs   <- map(SrcExpr, str = exprs$text, sloc = slocs)
    sexprs   <- sexprs[vapply1l(sexprs, isTranslateCall)]
    strans   <- lapply(sexprs, asSrcTranslation)
    sstrings <- lapply(sexprs, asSrcString) # TODO: to be implemented.

    attr(srcStrings, "path")      <- path
    attr(srcStrings, "basename")  <- basename(path)
    attr(srcStrings, "dirname")   <- dirname(path)
    attr(srcStrings, "timestamp") <- file.mtime(path)
    return(srcStrings)
}
