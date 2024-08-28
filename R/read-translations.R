read_translations <- function(file_path = "") {
    src_lines  <- readLines(file_path, encoding = "UTF-8")
    src_head   <- extract_src_translations_header(src_lines)
    src_blocks <- extract_src_translations_blocks(src_lines)

    header <- parse_src_translations_header(src_head)
    blocks <- parse_src_translations_blocks(src_blocks)
    return(TranslationsEnv(header = header, env = env))
}


# Internal extraction and parsing mechanisms -----------------------------------


extract_src_translations_header <- function(x = character()) {
    # We process the first pair of
    # separators and ignore the rest.
    sep_pos  <- grep("^---$", x)[c(1L, 2L)]
    na_count <- sum(is.na(sep_pos))
    indices  <- switch(na_count + 1L,
        # Case 1: na_count = 0: there is a header.
        seq.int(sep_pos[[1L]], sep_pos[[2L]]),
        # Case 2: na_count = 1: missing separator is treated as a format error.
        stops(
            "header's format is invalid. It misses a separator ('---'). ",
            "Each separator must be on its own line to be detected."),
        # Case 3: na_count = 2: there is no header at all.
        stops("a header is always required. Regenerate the underlying file."))

    # A non-empty header must span at least 3 lines.
    # Else, it is empty and {} is returned. This is
    # because jsonlite::parse_json() throws an error
    # when parsing an empty string.
    n_indices <- length(indices)
    return(if (n_indices > 2L) x[indices[-c(1L, n_indices)]] else "{}")
}

parse_src_translations_header <- function(x = character()) {
    fields <- jsonlite::parse_json(x, simplifyVector = TRUE)

    # JSON objects are parsed as named lists by
    # default. In the case of language_keys, it
    # is better to work with a named character.
    fields$language_keys <- unlist(fields$language_keys)
    return(do.call(translations_header, fields))
}

extract_src_translations_blocks <- function(x = character()) {
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

parse_src_translations_block <- function(x = character()) {
    id        <- parse_src_translation_block_id(x[[1L]])
    src_trans <- extract_src_translations(x)

    # Drop NULL block (source language's block).
    trans <- lapply(src_trans, parse_src_translation)
    trans <- trans[!vapply_1l(trans, is.null)]

    return(new_translation_block(id, trans))
}

parse_src_translation_block_id <- function(x = character(1L)) {
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

parse_src_translation <- function(x = character()) {
    # Source translation is always ignored and
    # discarded, because the underlying source
    # text (as written in the code) is used at
    # runtime for efficiency.
    if (is.null(lang <- parse_src_lang(x[[1L]]))) {
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

parse_src_lang <- function(x = character(1L)) {
    # Brackets are disallowed. They are reserved for
    # the source language's key which is always ignored.
    if (grepl("[{}]+", x)) {
        return(NULL)
    }

    # Remove all spaces and Markdown H2 title token
    # (##). What remains is the user's language key.
    return(gsub("[ \t#]*", "", x))
}
