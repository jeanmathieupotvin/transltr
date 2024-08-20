newTransltrFile <- function(path = "", dir = ".", fsep = .Platform$file.sep) {
    assertNonEmptyString(path)
    assertNonEmptyString(dir)
    on.exit(close(con))

    if (utils::file_test("-f", path <- normalizePath(path, fsep, FALSE))) {
        # TODO: manage existing file.
    }

    files   <- listSourceFiles(dir)
    strings <- extract(files, fsep = fsep)
    strings <- unlist(strings[lengths(strings) > 0L], FALSE, TRUE)

    # TODO: merge identical hashes
    a <- split(strings, names(strings))
    a <- a[lengths(a) > 1L]
    x <- a[[1L]]
    browser()

    lines <- unlist(lapply(strings, SrcStringToMd), TRUE, FALSE)
    con   <- file(path, "a+", TRUE, "UTF-8")
    base::writeLines(lines, con, sep = "\n")
    return(invisible(lines))
}

listSourceFiles <- function(path = ".") {
    assertNonEmptyString(path)
    return(
        list.files(
            path         = path,
            pattern      = "\\.[Rr]$",
            all.files    = TRUE,
            full.names   = FALSE,
            recursive    = TRUE,
            ignore.case  = FALSE,
            include.dirs = TRUE,
            no..         = TRUE))
}
