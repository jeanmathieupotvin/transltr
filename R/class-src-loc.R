SrcLoc <- function(
    file  = "",
    line1 = 0L,
    col1  = 0L,
    line2 = 0L,
    col2  = 0L)
{
    if (!isString(file)) {
        halt("'file' must be a non-NA character string.")
    }
    if (nzchar(file) && (
        !isSingleIntInRange(line1, 1L) ||
        !isSingleIntInRange(col1,  1L) ||
        !isSingleIntInRange(line2, 1L) ||
        !isSingleIntInRange(col2,  1L))) {
        halt("'line1', 'col1', 'line2', and 'col2' must all be non-NA integer values of length 1 strictly greater than 0.")
    }

    file <- normalizePath(file, .Platform$file.sep, FALSE)
    return(newSrcLoc(file, c(line1, col1), c(line2, col2)))
}

newSrcLoc <- function(file = "", start = integer(2L), end = integer(2L)) {
    return(structure(as.list(environment()), class = c("SrcLoc", "list")))
}

isSrcLoc <- function(x) {
    return(inherits(x, "SrcLoc"))
}

#' @export
format.SrcLoc <- function(x, ...) {
    if (!nzchar(x$file)) {
        return("<SrcLoc> no source file specified")
    }

    return(
        sprintf(
            "<SrcLoc> '%s': ln %i, col %i @@ ln %i, col %i",
            x$file,
            x$start[[1L]],
            x$start[[2L]],
            x$end[[1L]],
            x$end[[2L]]))
}

#' @export
print.SrcLoc <- function(x, ...) {
    cat(format(x, ...), "\n", sep = "")
    return(invisible(x))
}
