#' Placeholder
#'
#' Placeholder for future documentation.
#'
#' @seealso [Translations Source Files],
#'   [read_translations()],
#'   [write_translations()],
#'   [extract_src_header()],
#'   [from_src_header()]
#'
#' @rdname src-blocks
#' @family source blocks mechanisms
#' @keywords internal
extract_src_blocks <- function(x = character()) {
    start <- grep("^\\#[ \t]*`\\{\\{", x)
    end   <- c(start - 1L, length(x))[-1L]
    seqs  <- .mapply(seq.int, list(start, end, 1L), list())
    return(lapply(seqs, \(seq) x[seq]))
}

#' @rdname src-blocks
#' @keywords internal
tokenize_src_block <- function(x = character()) {
    # FIXME: validate me.
    # We need 1 more slot to initialize the
    # underlying finite-state machine (FSM).
    x   <- c("", x)
    x_n <- length(x)

    # This corresponds to an empty x. We
    # return an empty list with no token.
    if (x_n == 1L) {
        return(list())
    }

    # Initialize tokens. We preallocate a vector to
    # avoid growing objects. t_x[[i + 1L]] holds the
    # token of x[[i]]. The FSM starts at TOKEN_NULL.
    t_null <- token()
    t_x    <- replicate(x_n, t_null, simplify = FALSE)

    # We use 3 indices, i_l being the central one.
    #   - i_l is a line index. Elements of x are actual source lines.
    #   - i_t tracks the current translation text.
    #   - i_f tracks the current file in source location(s).
    # i_t and i_f are used as subtypes for LOC_FILE, LOC_RANGE,
    # and TXT_TRL tokens. This is useful for grouping purposes.
    # See below.
    i_l <- 2L
    i_t <- 0L
    i_f <- 0L

    while (i_l <= x_n) {
        x_i    <- x[[i_l]]
        t_prev <- t_x[[i_l - 1L]][["type"]]
        t_type <- if (grepl("^\\#[ \t]*`\\{\\{", x_i)) {
            "TOKEN_TITLE_HASH"
        } else if (grepl("^`(.*?)`:$", x_i)) {
            i_f <- i_f + 1L
            "TOKEN_LOC_FILE"
        } else if (grepl("^[ \t]*-[ \t]*line[ \t]+[0-9]+,[ \t]*column[ \t]+[0-9]+ @ line[ \t]+[0-9]+,[ \t]*column[ \t]+[0-9]+$", x_i)) {
            "TOKEN_LOC_RNG"
        } else if (grepl("^\\#\\#[ \t]*`\\{\\{", x_i)) {
            "TOKEN_TITLE_SRC_KEY"
        } else if (grepl("^\\#\\#", x_i)) {
            i_t <- i_t + 1L
            "TOKEN_TITLE_TXT_KEY"
        } else {
            # In other cases, current token is
            # derived from the previous token.
            switch(t_prev,
                TOKEN_NULL          = "TOKEN_NULL",
                TOKEN_TITLE_HASH    = "TOKEN_LOC_FILE",
                TOKEN_LOC_FILE      = "TOKEN_LOC_RNG",
                TOKEN_LOC_RNG       = "TOKEN_LOC_RNG",
                TOKEN_TITLE_SRC_KEY = "TOKEN_TXT_SRC",
                TOKEN_TXT_SRC       = "TOKEN_TXT_SRC",
                # The default case covers
                # TOKEN_TXT_TRL       -> TOKEN_TXT_TRL, and
                # TOKEN_TITLE_TXT_KEY -> TOKEN_TXT_TRL.
                "TOKEN_TXT_TRL")
        }

        t_sub <- switch(t_type,
            TOKEN_LOC_FILE = i_f,  # Assign an ID to each source file token.
            TOKEN_LOC_RNG  = i_f,  # Assign a file to each source range token.
            TOKEN_TXT_TRL  = i_t,  # Assign an ID to each translation.
            NULL)

        t_x[[i_l]] <- token(t_type, x_i, i_l, t_sub)
        i_l <- i_l + 1L
    }

    names(t_x) <- vapply_1c(t_x, `[[`, i = "type")

    # No need to return the first NULL token
    # used only to initialize the FSM.
    return(t_x[-1L])
}

parse_src_block <- function(tokens = list()) {
    # FIXME: write me.
    tokens <- tokenize_src_block(extract_src_blocks(readLines("inst/templates/v1-example.md"))[[1L]])
    t_vals <- names(tokens)

    hash      <- vapply_1c(tokens[t_vals == "TOKEN_TITLE_HASH"],    parse_src_block_title)
    text_key  <- vapply_1c(tokens[t_vals == "TOKEN_TITLE_SRC_KEY"], parse_src_block_title)
    lang_keys <- vapply_1c(tokens[t_vals == "TOKEN_TITLE_TXT_KEY"], parse_src_block_title)
    text      <- lapply(tokens[t_vals == "TOKEN_TXT_SRC"], parse_src_block_text)

    # TODO: i am pretty much here.
    t_subs <- vapply_1i(a, `[[`, i = "subtype")
    t_vals <- vapply_1c(a, `[[`, i = "value")
    trans  <- split(t_vals, t_subs)

    trans     <- lapply(trans, parse_src_block_text)
    names(trans) <- lang_keys
    return(block(hash, text, text_key, list(location()), trans))
}

parse_token <- function(token = token()) {
    # FIXME: validate me.
    return(
        switch(
            token$type,
            #TOKEN_TITLE_HASH    = parse_src_block_title(token),
            TOKEN_LOC_FILE      = parse_src_block_loc_file(token),
            TOKEN_LOC_RNG       = parse_src_block_loc_rng(token),
            #TOKEN_TITLE_SRC_KEY = parse_src_block_title(token),
            #TOKEN_TITLE_TXT_KEY = parse_src_block_title(token),
            TOKEN_TXT_SRC       = parse_src_block_text(token),
            TOKEN_TXT_TRL       = parse_src_block_text(token),
            NULL))
}

parse_src_block_title <- function(token = token()) {
    # FIXME: validate me.
    BLOCK_TITLE_TOKENS <- c("#", "`", "{", "}", " ")
    return(strip_chars(token$value, BLOCK_TITLE_TOKENS))
}

parse_src_block_text <- function(tokens = list()) {
    x <- vapply

    # Replace empty lines with the equivalent of a blank line.
    pos_blanks    <- which(!nzchar(x))
    x[pos_blanks] <- "\n\n"

    # Add a single space to the end of each line unless
    # (1) it is a blank line, (2) it precedes one, or
    # (3) it is the last line.
    pos_new_lines    <- which(endsWith(x, "\n"))
    pos_no_space     <- c(pos_new_lines, pos_new_lines - 1L, length(x))
    x[-pos_no_space] <- paste0(x[-pos_no_space], " ")

    return(paste0(x, collapse = ""))
}

parse_src_block_loc_file <- function(token = token()) {
    # FIXME: write me.
    return(NULL)
}

parse_src_block_loc_rng <- function(token = token()) {
    # FIXME: write me.
    # strsplit(gsub("[ \t]+|line|column|-|`|:", "", x), ",|@")
    return(NULL)
}

token <- function(type = "TOKEN_NULL", value = "", i_line = 0L, subtype = NULL) {
    # FIXME: validate me.
    return(
        structure(
            list(
                type    = type,
                subtype = subtype,
                value   = value,
                i_line  = i_line,
                ncols   = nchar(value, "chars")),
            class = c("Token", "list")))
}
