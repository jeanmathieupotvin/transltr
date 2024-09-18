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
#' @rdname from-tsf
#' @family translations source files mechanisms
#' @keywords internal
from_tsf <- function(x = character()) {
    src_parts <- split_tsf(x)
    header    <- from_src_header(src_parts$HEADER)
    blocks    <- from_src_blocks(src_parts$BLOCKS, header$template_version)
    return(list(header = header, blocks = blocks))
}

#' @rdname from-tsf
#' @keywords internal
split_tsf <- function(x = character()) {
    assert_chr(x, TRUE)
    x_split <- split(x, cumsum(grepl("^\\#[ \t]*`\\{\\{", x)))
    names(x_split) <- NULL
    return(
        list(
            HEADER = x_split[[1L]],
            BLOCKS = x_split[-1L]))
}

#' @rdname from-tsf
#' @keywords internal
from_src_header <- function(x = character()) {
    .cond_callback <- \(cond) {
        stopf(
            "header could not be read. The parser returned this error:\n! %s.",
            cond$message)
    }

    fields <- tryCatch(error = .cond_callback, warning = .cond_callback, {
        yaml::yaml.load(x,
            # eval.expr is always disallowed for better security.
            eval.expr     = FALSE,
            as.named.list = TRUE,
            merge.warning = TRUE)
    })

    template_version <- fields$template_version
    assert_match(template_version, get_template_versions())

    # No need to add a default case because
    # template_version is valid (see above).
    return(
        switch(
            template_version,
            do.call(from_src_header_v1, fields)))
}

#' @rdname from-tsf
#' @keywords internal
from_src_blocks <- function(
    blocks           = list(),
    template_version = get_template_versions())
{
    assert_arg(template_version)
    return(
        switch(
            template_version,
            from_src_blocks_v1(blocks)))
}

#' @usage
#' from_src_header_v1(
#'   template_version = 1L,
#'   generated_by     = get_generated_by(),
#'   generated_on     = get_generated_on(),
#'   hash_algorithm   = get_hash_algorithms(),
#'   hash_length      = 32L,
#'   language_keys    = list(en = "English"),
#'   ...
#' )
#'
#' @rdname from-tsf
#' @keywords internal
from_src_header_v1 <- function(
    template_version = 1L,
    generated_by     = get_generated_by(),
    generated_on     = get_generated_on(),
    hash_algorithm   = get_hash_algorithms(),
    hash_length      = 32L,
    language_keys    = list(en = "English"),
    ...)
{
    # YAML maps are parsed as named lists by
    # default. In the case of language_keys,
    # it is better to have a named character.
    language_keys <- unlist(language_keys)

    assert_chr1(generated_by)
    assert_chr1(generated_on)
    assert_arg(hash_algorithm, TRUE)
    assert_chr(language_keys, TRUE)
    assert_named(language_keys)
    assert_int1(hash_length)

    # Check that hash_length matches what
    # the chosen hashing algorithm expects.
    length_range <- get_hash_length_range(hash_algorithm)
    assert_between(hash_length, length_range[["min"]], length_range[["max"]])

    if (!is_named(further_fields <- list(...))) {
        stops("all further fields (custom user's fields) must be named.")
    }

    return(
        list(
            template_version = 1L,
            generated_by     = generated_by,
            generated_on     = generated_on,
            hash_algorithm   = hash_algorithm,
            hash_length      = hash_length,
            language_keys    = language_keys,
            further_fields   = further_fields))
}

#' @rdname from-tsf
#' @keywords internal
from_src_blocks_v1 <- function(blocks = list()) {
    blocks_t <- lapply(blocks, tokenize_src_block_v1)
    return(lapply(blocks_t, from_src_block_v1))
}

#' @rdname from-tsf
#' @keywords internal
tokenize_src_block_v1 <- function(x = character()) {
    assert_chr(x, TRUE)

    # Add 1 more (NULL) slot to
    # properly initialize the loop.
    x   <- c("", x)
    x_n <- length(x)

    # This corresponds to an empty x. We
    # return an empty list with no token.
    if (x_n == 1L) {
        return(list())
    }

    # Preaollocate a list for tokens. t_x[[i + 1L]]
    # holds the token of x[[i]]. t_x[[1L]] is left
    # as a NULL token because the loop below checks
    # the type of the (i - 1) token. It is dropped
    # before returning.
    t_null <- src_block_line_token("NULL")
    t_sub  <- "NULL"
    t_x    <- replicate(x_n, t_null, simplify = FALSE)

    # We use 3 indices, i_x being the central one.
    #   - i_x tracks the current x element. It starts at 2
    #     because the first element is a dummy NULL token.
    #   - i_l tracks the current source location.
    #   - i_t tracks the current translation subsection.
    i_l <- 0L
    i_t <- 0L
    x_s <- seq_len(x_n)[-1L]

    for (i_x in x_s) {
        x_i <- x[[i_x]]

        # Skip empty lines that does not immediately
        # follow a TXT token/line. Such elements are
        # not useful for parsing purposes. However,
        # spaces within streams of TXT matter because
        # they are part of the underlying text.
        if (!nzchar(x_i) && t_x[[i_x - 1L]]$type != "TXT") {
            next
        }

        x_type <- if (grepl("^\\#[ \t]*`\\{\\{", x_i)) {
            "TITLE_HASH"
        } else if (grepl("^`(.*?)`:$", x_i)) {
            i_l <- i_l + 1L
            "LOC_SRC_PATH"
        } else if (grepl("^[ \t]*-[ \t]*line[ \t]+[0-9]+,[ \t]*column[ \t]+[0-9]+ @ line[ \t]+[0-9]+,[ \t]*column[ \t]+[0-9]+$", x_i)) {
            "LOC_SRC_RNG"
        } else if (grepl("^\\#\\#[ \t]*`\\{\\{", x_i)) {
            txt_sub <- "TXT_SRC"
            "TITLE_KEY_SRC"
        } else if (grepl("^\\#\\#", x_i)) {
            txt_sub <- paste0("TXT_TRL_", i_t <- i_t + 1L)
            "TITLE_KEY_TXT"
        } else {
            "TXT"
        }

        # Subtypes are updated whenever a new TITLE_*_KEY
        # or LOC_SRC_PATH token is encountered. They both
        # signal that the nexts tokens are related.
        x_sub <- switch(x_type,
            LOC_SRC_PATH = paste0("LOC_SRC_", i_l),
            LOC_SRC_RNG  = paste0("LOC_SRC_", i_l),
            TXT          = txt_sub,
            NULL)

        t_x[[i_x]] <- src_block_line_token(x_type, x_i, x_sub)
    }

    # Drop superfluous NULL token.
    return(t_x[-1L])
}

#' @rdname from-tsf
#' @keywords internal
from_src_block_v1 <- function(tokens = list()) {
    t_split   <- split(tokens, vapply_1c(tokens, `[[`, i = "subtype"))
    t_hash    <- t_split$TITLE_HASH[[1L]]
    t_key_src <- t_split$TITLE_KEY_SRC[[1L]]
    a_key_txt <- t_split$TITLE_KEY_TXT
    a_src_txt <- t_split$TXT_SRC
    a_t_txt   <- t_split[grepl("^TXT_TRL_", names(t_split))]
    a_t_loc   <- t_split[grepl("^LOC_SRC_", names(t_split))]

    translations        <- lapply(a_t_txt, from_src_block_txt_v1)
    names(translations) <- lapply(a_key_txt, from_src_block_title_v1)

    return(
        block(
            hash         = from_src_block_title_v1(t_hash),
            text         = from_src_block_txt_v1(a_src_txt),
            text_key     = from_src_block_title_v1(t_key_src),
            locations    = lapply(a_t_loc, from_src_block_loc_v1),
            translations = translations))
}

#' @rdname from-tsf
#' @keywords internal
from_src_block_title_v1 <- function(token = src_block_line_token("TITLE_HASH")) {
    return(gsub("[#`{} \t]+", "", token$value))
}

#' @rdname from-tsf
#' @keywords internal
from_src_block_txt_v1 <- function(tokens = list()) {
    t_vals <- vapply_1c(tokens, `[[`, i = "value")
    text   <- strip_empty_strings(t_vals)

    # Identify lines that require a new line
    # character for appending (empty lines).
    i_nl <- which(!nzchar(text))

    # Identify lines that do not require a space
    # for appending: the last line, blank lines,
    # or lines that precede a blank one.
    i_no_s <- c(i_nl, i_nl - 1L, length(text))

    text[i_nl]    <- "\n"
    text[-i_no_s] <- paste0(text[-i_no_s], " ")

    return(paste0(text, collapse = ""))
}

#' @rdname from-tsf
#' @keywords internal
from_src_block_loc_v1 <- function(tokens = list()) {
    t_types <- vapply_1c(tokens, `[[`, i = "type")
    t_file  <- tokens[t_types == "LOC_SRC_PATH"][[1L]]
    t_rngs  <- tokens[t_types == "LOC_SRC_RNG"]
    l_rngs  <- lapply(t_rngs, from_src_block_loc_range_v1)

    return(
        location(
            path  = from_src_block_loc_path_v1(t_file),
            line1 = vapply_1i(l_rngs, `[[`, i = "line1"),
            col1  = vapply_1i(l_rngs, `[[`, i = "col1"),
            line2 = vapply_1i(l_rngs, `[[`, i = "line2"),
            col2  = vapply_1i(l_rngs, `[[`, i = "col2")))
}

#' @rdname from-tsf
#' @keywords internal
from_src_block_loc_path_v1 <- function(token = src_block_token("LOC_SRC_PATH")) {
    return(gsub("[` \t]+|:$", "", token$value))
}

#' @rdname from-tsf
#' @keywords internal
from_src_block_loc_range_v1 <- function(token = src_block_token("LOC_SRC_RNG")) {
    t_value <- token$value

    # Extract raw digits from string.
    starts <- gregexpr("([0-9]+)", t_value)[[1L]]
    ends   <- starts + attr(starts, "match.length") - 1L
    strs   <- substring(t_value, starts, ends)

    # Attempt to parse raw digits as integers.
    # Warnings are superfluous because an error
    # is thrown below if something goes wrong.
    ints <- suppressWarnings(as.integer(strs))

    if (length(ints) != 4L || anyNA(ints)) {
        stopf(
            "the following source location's range could not be read:\n'%s'.",
            gsub("^[ \t]+- ", "", token$value))
    }

    names(ints) <- c("line1", "col1", "line2", "col2")
    return(ints)
}

#' @rdname from-tsf
#' @keywords internal
src_block_line_token <- function(
    type = c(
        "NULL",
        "TITLE_HASH",
        "TITLE_KEY_SRC",
        "TITLE_KEY_TXT",
        "LOC_SRC_PATH",
        "LOC_SRC_RNG",
        "TXT"),
    value   = "",
    subtype = NULL)
{
    assert_arg(type, TRUE)
    assert_chr1(value, TRUE)
    assert_chr1(subtype <- subtype %||% type, TRUE)
    return(
        token(
            type,
            value,
            subtype = subtype,
            super   = "SrcBlockLineToken"))
}
