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

    assertSingleIntInRange(line1, 1L)
    assertSingleIntInRange(col1,  1L)
    assertSingleIntInRange(line2, 1L)
    assertSingleIntInRange(col2,  1L)

    return(newSrcLoc(file, c(line1, col1), c(line2, col2)))
}

newSrcLoc <- function(file = File(), start = integer(2L), end = integer(2L)) {
    return(structure(as.list(environment()), class = c("SrcLoc", "list")))
}

isSrcLoc <- function(x) {
    return(inherits(x, "SrcLoc"))
}

#' @export
format.SrcLoc <- function(x, class = TRUE, relPath = FALSE, ...) {
    if (is.null(x$file)) {
        return("<SrcLoc> no File specified")
    }

    assertSingleLgl(class)
    assertSingleLgl(relPath)

    return(
        sprintf(
            "%s'%s': ln %i, col %i @@ ln %i, col %i",
            if (class) "<SrcLoc> " else "",
            if (relPath) getFileRelPath(x$file) else x$file$path,
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
