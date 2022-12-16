newSrcLocation <- function(x, ...) {
    return(UseMethod("newSrcLocation"))
}

newSrcLocation.data.frame <- function(
    x,
    start = integer(1L),
    end   = integer(1L),
    ...)
{
    validateTokens(x)

    startPos <- tokens[start, c("line1", "col1"), drop = TRUE]
    endPos   <- tokens[end,   c("line2", "col2"), drop = TRUE]
    srcLoc   <- c(startPos, endPos)

    storage.mode(srcLoc) <- "integer"
    return(structure(srcLoc, class = "SrcLocation"))
}

isSrcLocation <- function(x) {
    return(inherits(x, "SrcLocation"))
}

newSrcText <- function(
    text     = character(1L),
    lang     = character(1L),
    file     = character(1L),
    location = newSrcLocation())
{
    validateString(text)
    validateString(lang)
    validateFile(file)

    if (!isSrcLocation(location)) {
        stopf("TypeError", "`location` must be a `SrcLocation` object.")
    }

    return(
        structure(
            as.list(environment()),
            class = c("SrcText", "list")))
}
