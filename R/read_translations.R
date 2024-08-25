file_path <- ".templates/v1-example.md"

read_translations_file <- function(file_path = "") {
    file_path  <- ".templates/v1-example.md"
    src_lines  <- readLines(file_path, encoding = "UTF-8")
    src_head   <- extract_src_translations_header(src_lines)
    header <- parse_src_translations_header(src_head$src)

    x <- src_lines[-src_head$range]
    src_trans <- extract_translations(src_lines[-src_head$range])
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

    if (na_count == 2L) {
        # If both separators are missing, then
        # we consider there is no header at all.
        h_range  <- 0L
        h_length <- 0L
    } else if (na_count == 1L) {
        # A single missing separator is a formatting error.
        stops("header is invalid because it misses a separator ('---').")
    } else {
        # Else, we extract whatever is between both separators.
        h_range  <- seq.int(sep_pos[[1L]], sep_pos[[2L]], 1L)
        h_length <- length(h_range)
    }

    # A non-empty header must span at
    # least 3 lines. Else, it is empty.
    return(
        list(
            src    = if (h_length > 2L) x[h_range[-c(1L, h_length)]] else "{}",
            range  = h_range,
            length = h_length))
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


extract_translations <- function(x = character()) {
    # The regular expression below matches
    # Markdown H1 titles with this format:
    # '# <line start><spaces>{{ <alphanumeric characters> }}<spaces><line end>'.
    # ^            : matches start of line
    # \\#          : matches character # literally
    # [ ]+         : matches one or more space characters
    # \\{          : matches character { literally
    # [ ]*         : matches zero or more space characters
    # [a-zA-Z0-9]+ : matches any alphanumeric (English) character (one or more)
    # \\}          : matches character } literally
    # $            : matches end of line
    b_start <- grep("^\\#[ ]+\\{\\{[ ]*[a-zA-Z0-9]+[ ]*\\}\\}[ ]*$", x)


    b_start
}

split_translations_blocks <- function(x = character()) {

}
