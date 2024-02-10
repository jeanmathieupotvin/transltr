File <- function(path = character(1L)) {
    if (!isNonEmptyString(path)) {
        halt("'path' must be a non-NA and non-empty character of length 1.")
    }
    if (!utils::file_test("-f", path <- normalizePath(path, "/", FALSE))) {
        halt("'%s' either does not exist or is not a valid file.", path)
    }

    info   <- file.info(path, extra_cols = FALSE)
    name   <- basename(path)
    dir    <- dirname(path)
    reldir <- gsub(sprintf("%s/", getwd()), "", dirname(path))

    timefmt  <- "%A, %Y-%m-%d, %T (%Z)"
    ctime    <- format(info$ctime, format = timefmt)
    mtime    <- format(info$mtime, format = timefmt)
    ctimeutc <- format(info$ctime, format = timefmt, tz = "UTC")
    mtimeutc <- format(info$mtime, format = timefmt, tz = "UTC")

    return(newFile(path, name, dir, reldir, ctime, mtime, ctimeutc, mtimeutc))
}

newFile <- function(
    path     = "",
    name     = "",
    dir      = "",
    reldir   = "",
    ctime    = "",
    mtime    = "",
    ctimeutc = "",
    mtimeutc = "")
{
    return(structure(as.list(environment()), class = c("File", "list")))
}

isFile <- function(x) {
    return(inherits(x, "File"))
}

#' @export
format.File <- function(x, ...) {
    return(
        formatNamedValues(
            File             = x$name,
            Path             = x$path,
            `Directory`      = x$dir,
            `Rel. Directory` = x$reldir,
            `Created`        = x$ctime,
            `Last Modified`  = x$mtime))
}

#' @export
print.File <- function(x, ...) {
    cat("<File>", format(x, ...), sep = "\n")
    return(invisible(x))
}
