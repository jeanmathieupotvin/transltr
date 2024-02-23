File <- function(path = character(1L), fsep = c("/", "\\")) {
    assertString(path)
    assertChoice(fsep)

    if (fsep == "/") {
        # Pre-sanitize path that contains \\
        # if chosen fsep is / for consistency.
        path <- gsub("\\\\", "/", path)
    }
    if (!utils::file_test("-f", path <- normalizePath(path, fsep, FALSE))) {
        halt("'%s' either does not exist or is not a valid file.", path)
    }

    name <- basename(path)
    info <- file.info(path, extra_cols = FALSE)
    size <- info[[1L, "size"]]

    return(
        newFile(
            name,
            getFileExt(name, .validate = FALSE),
            size,
            hashFile(path, size, .validate = FALSE),
            dirname(path),
            path,
            fsep,
            info[[1L, "mtime"]]))
}

newFile <- function(
    name  = "",
    ext   = "",
    size  = 0.0,
    hash  = "",
    dir   = "",
    path  = "",
    fsep  = c("/", "\\"),
    mtime = Sys.time())
{
    return(structure(as.list(environment(), TRUE), class = c("File", "list")))
}

isFile <- function(x) {
    return(inherits(x, "File"))
}

#' @export
format.File <- function(x, utc = FALSE, format = "%A, %Y-%m-%d, %T (%Z)", ...) {
    assertSingleLgl(utc)
    assertNonEmptyString(format)

    return(c(
        formatNamedValues(
            Name            = x$name,
            Extension       = x$ext,
            Size            = sprintf("%1.0f bytes", x$size),
            Hash            = x$hash,
            Directory       = x$dir,
            Path            = x$path,
            `Last Modified` = format(x$mtime,
                tz     = if (utc) "UTC" else "",
                format = format)),
        formatNamedValues(
            # Separate fields from useful
            # values computed on the fly.
            `---` = "",
            `Current Working Directory` = getwd(),
            `Current Relative Path`     = getFileRelPath(x$path))))
}

#' @export
print.File <- function(x, ...) {
    cat("<File>", format(x, ...), sep = "\n")
    return(invisible(x))
}

hashFile <- function(x, ...) {
    UseMethod("hashFile")
}

#' @export
hashFile.File <- function(x, ...) {
    return(hashFile(x$path, x$size, ...))
}

#' @export
hashFile.character <- function(x = "", size = NULL, .validate = TRUE, ...) {
    if (.validate) {
        size <- size %??% file.info(x, extra_cols = FALSE)[[1L, "size"]]
        assertNonEmptyString(x)
        assertSingleDblInRange(size, 0L)
    }

    return(sodium::bin2hex(sodium::sha256(readBin(x, "raw", size))))
}

getFileExt <- function(x, ...) {
    UseMethod("getFileExt")
}

#' @export
getFileExt.File <- function(x, ...) {
    return(getFileExt(x$path, ...))
}

#' @export
getFileExt.character <- function(x, .validate = TRUE, ...) {
    if (.validate) {
        assertNonEmptyString(x)
        x <- basename(x)
    }

    start <- regexpr("\\.(.*?)$", x) + 1L
    return(substr(x, start, start + attr(start, "match.length")))
}

getFileRelPath <- function(x, ...) {
    UseMethod("getFileRelPath")
}

#' @export
getFileRelPath.File <- function(x, ...) {
    return(getFileRelPath(x$path, x$fsep, ...))
}

#' @export
getFileRelPath.character <- function(x, fsep = c("/", "\\"), .validate = TRUE, ...) {
    if (.validate) {
        assertNonEmptyString(x)
        assertChoice(fsep)
        x    <- normalizePath(x, fsep, FALSE)
    }

    # If x is not within the current working
    # directory's scope, we do not attempt to
    # find x's relative position to it. Such
    # an operation is tedious and error-prone.
    if (!grepl(wd <- getwd(), x)) {
        return("<undetermined>")
    }

    # file.path(wd, "") is an efficient
    # way to add a trailing slash to wd.
    return(gsub(file.path(wd, "", fsep = fsep), "", x))
}
