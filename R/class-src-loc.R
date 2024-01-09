newSrcLoc <- function(
    file  = "",
    line1 = 0L,
    col1  = 0L,
    line2 = 0L,
    col2  = 0L)
{
    if (!isString(file)) {
        stopf("'file' must be a non-NA character string.")
    }
    if (nzchar(file) && (
        !isSingleIntegerInRange(line1, 1L) ||
        !isSingleIntegerInRange(col1,  1L) ||
        !isSingleIntegerInRange(line2, 1L) ||
        !isSingleIntegerInRange(col2,  1L))) {
        stopf(
            "'line1', 'col1', 'line2', and 'col2' must all be",
            "non-NA integer values of length 1 strictly greater than 0.")
    }

    obj <- list(
        file   = normalizePath(file, .Platform$file.sep, mustWork = FALSE),
        start  = c(line1, col1),
        end    = c(line2, col2))

    class(obj) <- c("SrcLoc", "list")
    return(obj)
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
        sprintf("<SrcLoc> '%s': ln %i, col %i @@ ln %i, col %i",
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
