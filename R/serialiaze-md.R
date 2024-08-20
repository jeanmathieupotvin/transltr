to_md.SrcLoc <- function(x, ...) {
    return(format(x, class = FALSE, relPath = TRUE))
}

to_md <- function(hash = "", slocs = list(), slang = "", str = "") {
    h1    <- new_title(hash, 1L)
    slocs <- new_block(unlist(lapply(slocs, to_md), use.names = FALSE))
    h2    <- new_title(slang, 2L)
    str   <- new_block_str(str)
    return(c(h1, slocs, h2, str))
}


# Helpers ----------------------------------------------------------------------


format_empty <- function(x, default = "<empty>") {
    return(if (any(nzchar(x))) x else default)
}

new_title <- function(x, level = 1L) {
    return(c(sprintf("%s %s", strrep("#", level), x), ""))
}

new_block <- function(x, indent = 4L) {
    return(c(paste0(strrep(" ", indent), x), ""))
}

new_block_str <- function(x) {
    return(new_block(format_empty(strwrap(x, width = 80L))))
}

