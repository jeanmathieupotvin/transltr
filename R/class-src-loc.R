SrcLoc <- function(
    file  = File(),
    line1 = 1L,
    col1  = 1L,
    line2 = 1L,
    col2  = 1L)
{
    if (!is.null(file) && !isFile(file)) {
        halt("'file' must be NULL or a File object.")
    }
    if (!isSingleIntInRange(line1, 1L) ||
        !isSingleIntInRange(col1,  1L) ||
        !isSingleIntInRange(line2, 1L) ||
        !isSingleIntInRange(col2,  1L)) {
        halt("'line1', 'col1', 'line2', and 'col2' must all be non-NA integer values of length 1 strictly greater than 0.")
    }

    return(newSrcLoc(file, c(line1, col1), c(line2, col2)))
}

newSrcLoc <- function(file = File(), start = integer(2L), end = integer(2L)) {
    return(structure(as.list(environment()), class = c("SrcLoc", "list")))
}

isSrcLoc <- function(x) {
    return(inherits(x, "SrcLoc"))
}

#' @export
format.SrcLoc <- function(x, ...) {
    if (is.null(x$file)) {
        return("<SrcLoc> no File specified")
    }

    return(
        sprintf(
            "<SrcLoc> '%s': ln %i, col %i @@ ln %i, col %i",
            x$file$path,
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
