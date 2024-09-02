extract_src_blocks <- function(x = character()) {
    # Blocks are identified with the following regular expression.
    # It identifies a Markdown H1 title enclosed in {{ }}.
    # ^            : matches start of line
    # \\#          : matches character # literally
    # [ ]+         : matches one or more space characters
    # \\{          : matches character { literally
    # [ ]*         : matches zero or more space characters
    # [a-zA-Z0-9]+ : matches any hexadecimal character (one or more)
    # \\}          : matches character } literally
    # $            : matches end of line
    start <- grep("^\\#[ \t]+\\{\\{[ \t]*[a-fA-F0-9]+[ \t]*\\}\\}[ \t]*$", x)

    # Each block starts at an index within b_start
    # and ends just before the next one. The last
    # block ends at the end of the vector/file.
    end     <- tail(c(start - 1L, length(x)), length(start))
    indices <- .mapply(seq.int, list(start, end), list())

    return(lapply(indices, \(i) x[i]))
}

from_src_block <- function(x = character()) {
    id        <- from_src_block_id(x[[1L]])
    src_trans <- extract_src_translations(x)
    trans     <- lapply(src_trans, from_src_translation)
    return(new_block(id, unlist(trans)))
}

from_src_block_id <- function(x = character(1L)) {
    start <- regexpr("[a-fA-F0-9]+", x)
    return(substr(x, start, start + attr(start, "match.length") - 1L))
}

extract_src_translations <- function(x = character()) {
    # Translations are identified with the following regular expression.
    # It identifies a Markdown H2 title possibly enclosed in {{ }}.
    # ^    : matches start of line
    # \\#  : matches character # literally
    # [ ]* : matches one or more space characters
    # .+   : matches any character (one or more)
    start <- grep("^\\#\\#[ \t]*.+$", x)

    # Each block starts at an index within b_start
    # and ends just before the next one. The last
    # block ends at the end of the vector/file.
    end     <- tail(c(start - 1L, length(x)), length(start))
    indices <- .mapply(seq.int, list(start, end), list())

    return(lapply(indices, \(i)  x[i]))
}

from_src_translation <- function(x = character()) {
    # Source translation is always ignored and
    # discarded, because the underlying source
    # text (as written in the code) is used at
    # runtime for efficiency.
    if (is.null(lang <- from_src_lang(x[[1L]]))) {
        return(NULL)
    }

    # Remove lang from x and all superfluous spaces
    # before and after contents (any non-empty lines).
    # Spaces intertwined within contents are kept.
    x     <- x[-1L]
    is_nz <- which(nzchar(x))
    x     <- x[seq.int(min(is_nz), max(is_nz))]

    return(new_translation(lang, paste0(x, collapse = "\n")))
}

from_src_lang <- function(x = character(1L)) {
    # Brackets are are reserved for the source
    # language's key which is always ignored.
    # Remove all spaces and Markdown H2 title token
    # (##). What remains is the user's language key.
    return(if (grepl("[{}]+", x)) NULL else gsub("[ \t#]*", "", x))
}
