read_translations_file <- function(file_path = "") {
    file_path  <- ".templates/v1-example.md"
    src_lines  <- readLines(file_path, encoding = "UTF-8")
    src_head   <- extract_src_translations_header(src_lines)
    src_blocks <- extract_src_translations_blocks(src_lines)

    header <- parse_src_translations_header(src_head$src)

    return(invisible())
}


# Header -----------------------------------------------------------------------


extract_src_translations_header <- function(x = character()) {
    # Header starts and ends with 3 dashes that
    # must be on separate lines. Whatever is in
    # between is the actual contents to parse.

    # We only process the first pair of separators.
    # Whatever comes after is ignored.
    sep_pos  <- grep("^---$", x)[c(1L, 2L)]
    na_count <- sum(is.na(sep_pos))
    indices  <- switch(na_count + 1L,
        # Case 1: na_count = 0: there is a header.
        seq.int(sep_pos[[1L]], sep_pos[[2L]]),
        # Case 2: na_count = 1: missing separator is treated as a formatting error.
        stops(
            "header's format is invalid. It misses a separator ('---'). ",
            "Each separator must be on its own line to be detected."),
        # Case 3: na_count = 2: there is no header at all.
        stops("a header is always required. Regenerate the underlying file."))

    # A non-empty header must span at least 3 lines.
    # Else, it is empty. "{}" is returned if header
    # is empty because jsonlite::parse_json() throws
    # an error when parsing an empty string.
    n_indices <- length(n_indices)
    return(if (n_indices > 2L) x[indices[-c(1L, n_indices)]] else "{}")
}

parse_src_translations_header <- function(x = character()) {
    return(
        new_translations_header(
            jsonlite::parse_json(x, simplifyVector = TRUE)))
}

new_translations_header <- function(...) {
    return(structure(list(...), class = c("TranslationsHeader", "list")))
}


# Translations -----------------------------------------------------------------


extract_src_translations_blocks <- function(x = character()) {
    # Each block starts with a Markdown H1
    # title having this standard format:
    # '# <line start>{{<alphanumeric characters>}}<line end>'.
    # Space(s) may be added before and/or after '{{' and '}}'.

    # Blocks are identified with the following regular expression.
    # ^            : matches start of line
    # \\#          : matches character # literally
    # [ ]+         : matches one or more space characters
    # \\{          : matches character { literally
    # [ ]*         : matches zero or more space characters
    # [a-zA-Z0-9]+ : matches any hexadecimal character (one or more)
    # \\}          : matches character } literally
    # $            : matches end of line
    start <- grep("^\\#[ ]+\\{\\{[ ]*[a-fA-F0-9]+[ ]*\\}\\}[ ]*$", x)

    # Each block starts at an index within b_start
    # and ends just before the next one. The last
    # block ends at the end of the vector/file.
    end     <- tail(c(start - 1L, length(x)), length(start))
    indices <- .mapply(seq.int, list(start, end), list())

    return(lapply(indices, \(i) x[i]))
}


    # Each block starts at an index within b_start
    # and ends just before the next one. The last
    # block ends at the end of the vector/file.
    b_end   <- tail(c(b_start - 1L, length(x)), length(b_start))
    b_range <- .mapply(seq.int, list(b_start, b_end), list())

    return(lapply(b_range, \(i) {
        list(
            src    = x[i],
            range  = i,
            length = length(i))
    }))
}

parse_src_translations_blocks <- function(...) {}
