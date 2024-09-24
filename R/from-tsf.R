#' Convert Translation Source Files Back to R Objects
#'
#' A collection of **internal** mechanisms to convert [Translations Source Files]
#' back into proper \R objects. Its contents is split, tokenized, parsed, and
#' validated according to an underlying `template_version`. Typical users should
#' always use [read_translations()] instead.
#'
#' @details
#' All functions above follow an implicit top-down hierarchy. Conceptually,
#' they break a [Translations Source File] into smaller components and
#' eventually convert them to \R objects. [from_tsf()] is the entry point as
#' the main function in charge of the conversion. It relies on [split_tsf()]
#' to separate header's source lines from blocks' source lines, and on
#' [from_tsf_header()] and [from_tsf_blocks()] to actually convert these source
#' lines into \R objects.
#'
#' ```
#' from_tsf()
#'   -> split_tsf()
#'   -> from_tsf_header()
#'   -> from_tsf_blocks()
#' ```
#'
#' ## Header
#'
#' Since the header is just a set of YAML lines, [from_tsf_header()] parses
#' them with [yaml::yaml.load()]. Parsed values are validated and transformed
#' by [`from_tsf_header_v*()`][from_tsf_header()] subfunctions.
#'
#' ```
#' from_tsf_header()
#'   -> from_tsf_header_v*()
#' ```
#'
#' ## Blocks
#'
#' Blocks consists of (at least) five main components formatted as Markdown
#' text. They are trickier to parse and validate (compared to headers) because
#' they are more complex by design.
#'
#' The highest-level functions are [`from_tsf_blocks_v*()`][from_tsf_blocks_v1()].
#' They are all vectorized versions of [`from_tsf_block_v*()`][from_tsf_block_v1()]
#' which converts a single set of source lines into a [`Block`][Block] object.
#'
#' ```
#' from_tsf_blocks()
#'   -> from_tsf_blocks_v*()
#'        -> tokenize_tsf_block_v*()
#'             -> tokenize_tsf_block_line_v*()
#'             -> tsf_block_line_token()
#'        -> from_tsf_block_v*()
#'             -> from_tsf_block_title_v*()
#'             -> from_tsf_block_txt_v*()
#'             -> from_tsf_block_loc_v*()
#'                  -> from_tsf_block_loc_path_v*()
#'                  -> from_tsf_block_loc_range_v*()
#' ```
#'
#' To ease the conversion and validation processes, source lines are first
#' tokenized. It is important to note that unlike typical tokenizers,
#' [`tokenize_tsf_block_v*()`][tokenize_tsf_block_v1()] tokenizes whole lines.
#' In the context of a TSF, source lines are the smallest units of meaningful
#' data (most of the time) and tokenizing them is *enough* for the purposes of
#' [`transltr`][transltr]. Future versions of the package may introduce a more
#' formal tokenization process.
#'
#' There are three main converters.
#'
#'   * [`from_tsf_block_title_v*()`][from_tsf_block_title_v1()] converts Markdown
#'     (H2) titles to proper language keys. See subsection *Language Keys* of
#'     [Translations Source Files] for more information.
#'
#'   * [`from_tsf_block_txt_v*()`][from_tsf_block_txt_v1()] converts Markdown
#'     text to proper \R character strings.
#'
#'   * [`from_tsf_block_loc_v*()`][from_tsf_block_loc_v1()] converts Markdown
#'     text to [`Location`][Location] objects.
#'
#' @param x A character vector of source (unparsed) text lines. There is one
#'   exception: [tokenize_tsf_block_line_v1()] expects a character string.
#'
#' @param src_blocks A list of character vector of source (unparsed) text lines.
#'   Each element represents a different source [`Block`][Block] object.
#'
#' @param ... Further header custom fields. They must always be named.
#'
#' @param tokens A list of [`BlockLineToken`][tsf_block_line_token()]
#'   objects. Some functions such as [from_tsf_block_txt_v1()] expect these
#'   tokens to be of the same type.
#'
#' @param token A single [`BlockLineToken`][tsf_block_line_token()] object.
#'
#' @template param-template-version
#'
#' @template param-generated-by
#'
#' @template param-generated-on
#'
#' @template param-hash-algorithm
#'
#' @template param-hash-length
#'
#' @template param-language-keys
#'
#' @returns
#' This section is split according to the top-down hierarchy explained above.
#'
#' [from_tsf()] returns a named list of length 3 containing the following
#' elements:
#'
#' \describe{
#'   \item{`header`}{The output of [from_tsf_header()].}
#'   \item{`blocks`}{The output of [from_tsf_blocks()].}
#'   \item{`rest`}{Remaining source lines, in order. These lines are not
#'     part of the header or any block. They are ignored and included as is.
#'   }
#' }
#'
#' ```
#' ---
#' ```
#'
#' [split_tsf()] returns a named list of length 3 containing the following
#' elements:
#'
#' \describe{
#'   \item{`header`}{A character vector. The source lines of the header.}
#'   \item{`blocks`}{A list of character vector. Each element holds the source
#'     lines of one block.}
#'   \item{`rest`}{Remaining source lines, in order. These are lines that are
#'     not part of the header or any block. They are ignored and included as is.
#'   }
#' }
#'
#' ```
#' ---
#' ```
#'
#' [from_tsf_header()] returns a named list. Its contents depends on the
#' underlying `template_version` value.
#'
#' [from_tsf_blocks()] returns a list containing [`Block`][Block] objects.
#'
#' ```
#' ---
#' ```
#'
#' [from_tsf_header_v1()] returns a named list of length 7 containing the
#' values of its arguments along with a `further_fields` element. The latter
#' is a named list holding values passed to `...`.
#'
#' [from_tsf_blocks_v1()] returns a list containing [`Block`][Block] objects.
#'
#' ```
#' ---
#' ```
#'
#' [from_tsf_block_v1()] returns a [`Block`][Block] object.
#'
#' [from_tsf_block_title_v1()] and [from_tsf_block_txt_v1()] returns a
#' character vector.
#'
#' [from_tsf_block_loc_v1()] returns a [`Location`][Location] object.
#'
#' [from_tsf_block_loc_path_v1()] returns a character string.
#'
#' [from_tsf_block_loc_range_v1()] returns a named integer vector of length
#' 4 containing the following elements: `line1`, `col1`, `line2`, and `col2`.
#'
#' ```
#' ---
#' ```
#'
#' [tokenize_tsf_block_v1()] returns a list of
#' [`BlockLineToken`][tsf_block_line_token()] objects. Its order matches
#' the implicit order of argument `x`. Its length is equal to the length of
#' `x`. `NULL` tokens are preserved even though they are (typically) never
#' useful.
#'
#' [tokenize_tsf_block_line_v1()] returns a single
#' [`BlockLineToken`][tsf_block_line_token()] object.
#'
#' @note
#' [from_tsf_header_v1()] assumes `template_version` is always equal to
#' `1` and ignores any value passed to it. The argument is included to ease the
#' implementation of [from_tsf_header()].
#'
#' @seealso [Translations Source Files],
#'   [read_translations()],
#'   [tsf_block_line_token()]
#'
#' @rdname from-tsf
#' @family translations source files mechanisms
#' @keywords internal
from_tsf <- function(x = character()) {
    x_split <- split_tsf(x)
    header  <- from_tsf_header(x_split$header)
    blocks  <- from_tsf_blocks(x_split$blocks, header$template_version)
    return(
        list(
            header = header,
            blocks = blocks,
            rest   = x_split$rest))
}

#' @rdname from-tsf
#' @keywords internal
split_tsf <- function(x = character()) {
    assert_chr(x, TRUE)

    x_grps  <- cumsum(grepl(.TSF_SRC_BLOCK_LINE_TOKEN_PATTERNS[["TITLE_HASH"]], x))
    x_split <- split_ul(x, x_grps)

    # Concatenating lines into a single string
    # makes it much easier to detect single line
    # headers. These are ugly but permitted here.
    rest <- paste0(x_split[[1L]], collapse = "\n")

    if (anyNA(h_pos <- gregexpr("---", rest)[[1L]][c(1L, 2L)])) {
        stops("invalid or missing header. A separator could be missing.")
    }

    # Split header from rest and drop separators.
    # Convert strings back to character vectors.
    h_no_sep <- strsplit(substr(rest, h_pos[[1L]] + 3L, h_pos[[2L]] - 1L), "\n")[[1L]]
    rest     <- strsplit(substr(rest, h_pos[[2L]] + 3L, nchar(rest)),      "\n")[[1L]]

    return(
        list(
            header = strip_empty_strings(h_no_sep),
            blocks = x_split[-1L],
            rest   = rest))
}

#' @rdname from-tsf
#' @keywords internal
from_tsf_header <- function(x = character()) {
    .stopf <- \(cond) {
        stopf(
            "header could not be read. The parser returned this error:\n! %s.",
            cond$message)
    }

    fields <- tryCatch(error = .stopf, warning = .stopf, {
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
            do.call(from_tsf_header_v1, fields)))
}

#' @rdname from-tsf
#' @keywords internal
from_tsf_blocks <- function(
    src_blocks       = list(),
    template_version = get_template_versions())
{
    assert_arg(template_version)
    return(
        switch(
            template_version,
            from_tsf_blocks_v1(src_blocks)))
}

#' @rdname from-tsf
#' @keywords internal
from_tsf_header_v1 <- function(
    template_version = 1L,
    generated_by,
    generated_on,
    hash_algorithm,
    hash_length,
    language_keys,
    ...)
{
    # Intercept missing header fields
    # and throw an error, if any.
    args_c <- names(sys.call())
    args_f <- c(
        "template_version",
        "generated_by",
        "generated_on",
        "hash_algorithm",
        "hash_length",
        "language_keys")

    if (any(is_miss <- is.na(match(args_f, args_c)))) {
        stopf(
            "incomplete header. These fields are required but missing: %s.",
            to_string(args_f[is_miss], TRUE, ", and "))
    }

    # YAML maps are parsed as named lists by
    # default. In the case of language_keys,
    # it is better to have a named character.
    language_keys <- unlist(language_keys)

    assert_chr1(generated_by)
    assert_chr1(generated_on)
    assert_chr(language_keys, TRUE)
    assert_named(language_keys)
    assert_int1(hash_length)

    len_rng <- get_hash_length_range(hash_algorithm)
    assert_between(hash_length, len_rng[["min"]], len_rng[["max"]])

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
from_tsf_blocks_v1 <- function(src_blocks = list()) {
    blocks_t <- lapply(src_blocks, tokenize_tsf_block_v1)
    return(lapply(blocks_t, from_tsf_block_v1))
}

#' @rdname from-tsf
#' @keywords internal
from_tsf_block_v1 <- function(tokens = list()) {
    t_split   <- split(tokens, vapply_1c(tokens, `[[`, i = "subtype"))
    t_hash    <- t_split$TITLE_HASH[[1L]]
    t_key_src <- t_split$TITLE_KEY_SRC[[1L]]
    a_key_txt <- t_split$TITLE_KEY_TXT
    a_tsf_txt <- t_split$TXT_SRC
    a_t_txt   <- t_split[grepl("^TXT_TRL_", names(t_split))]
    a_t_loc   <- t_split[grepl("^LOC_SRC_", names(t_split))]

    translations        <- lapply(a_t_txt,   from_tsf_block_txt_v1)
    names(translations) <- lapply(a_key_txt, from_tsf_block_title_v1)
    names(a_t_loc)      <- NULL

    return(
        block(
            hash         = from_tsf_block_title_v1(t_hash),
            text         = from_tsf_block_txt_v1(a_tsf_txt),
            text_key     = from_tsf_block_title_v1(t_key_src),
            locations    = lapply(a_t_loc, from_tsf_block_loc_v1),
            translations = translations))
}

#' @rdname from-tsf
#' @keywords internal
from_tsf_block_title_v1 <- function(token = tsf_block_line_token("TITLE_HASH")) {
    return(gsub("[#`{} \t]+", "", token$value))
}

#' @rdname from-tsf
#' @keywords internal
from_tsf_block_txt_v1 <- function(tokens = list()) {
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
from_tsf_block_loc_v1 <- function(tokens = list()) {
    t_types <- vapply_1c(tokens, `[[`, i = "type")
    t_file  <- tokens[t_types == "LOC_SRC_PATH"][[1L]]
    t_rngs  <- tokens[t_types == "LOC_SRC_RNG"]
    l_rngs  <- lapply(t_rngs, from_tsf_block_loc_range_v1)

    return(
        location(
            path  = from_tsf_block_loc_path_v1(t_file),
            line1 = vapply_1i(l_rngs, `[[`, i = "line1"),
            col1  = vapply_1i(l_rngs, `[[`, i = "col1"),
            line2 = vapply_1i(l_rngs, `[[`, i = "line2"),
            col2  = vapply_1i(l_rngs, `[[`, i = "col2")))
}

#' @rdname from-tsf
#' @keywords internal
from_tsf_block_loc_path_v1 <- function(token = tsf_block_line_token("LOC_SRC_PATH")) {
    return(gsub("[` \t]+|:$", "", token$value))
}

#' @rdname from-tsf
#' @keywords internal
from_tsf_block_loc_range_v1 <- function(token = tsf_block_line_token("LOC_SRC_RNG")) {
    t_value <- token$value

    # Extract raw digits. It is easier
    # to remove the starting dash,
    # blank characters, and then
    # strip the remaining tokens.
    strs <- strsplit(gsub("^[ \t]*-|[ \t]*", "", t_value), "line|column|@|,")[[1L]]

    # Attempt to parse raw digits as integers.
    # Warnings are superfluous because an error
    # is thrown below if something goes wrong.
    ints <- suppressWarnings(as.integer(strs[nzchar(strs)]))

    if (length(ints) != 4L || anyNA(ints) || !all(ints > 0L)) {
        stopf(
            "the following source location's range could not be converted:\n'%s'.",
            gsub("^[ \t]+- ", "", token$value))
    }

    names(ints) <- c("line1", "col1", "line2", "col2")
    return(ints)
}

#' @rdname from-tsf
#' @keywords internal
tokenize_tsf_block_v1 <- function(x = character()) {
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
    t_null  <- tsf_block_line_token("NULL")
    t_x     <- replicate(x_n, t_null, simplify = FALSE)
    txt_sub <- "NULL"

    # We use 3 indices, i_x being the central one.
    #   - i_x tracks the current x element. It starts at 2
    #     because the first element is a dummy NULL token.
    #   - i_l tracks the current source location.
    #   - i_t tracks the current translation subsection.
    i_l    <- 0L
    i_t    <- 0L
    x_span <- seq.int(2L, x_n)

    for (i_x in x_span) {
        x_i    <- x[[i_x]]
        t_prev <- t_x[[i_x - 1L]]

        # Only empty lines that are part of TXT hunks
        # matter. Others are tokenized as NULL.
        if (!nzchar(x_i) && t_prev$type != "TXT") next

        x_type <- tokenize_tsf_block_line_v1(x_i)

        # Update current subtype for TXT tokens.
        # It is only updated whenever a TITLE_KEY_*
        # token is encountered.
        txt_sub <- switch(x_type,
            TITLE_KEY_SRC = "TXT_SRC",
            TITLE_KEY_TXT = paste0("TXT_TRL_", i_t <- i_t + 1L),
            txt_sub)

        x_sub <- switch(x_type,
            LOC_SRC_PATH = paste0("LOC_SRC_", i_l <- i_l + 1L),
            LOC_SRC_RNG  = paste0("LOC_SRC_", i_l),
            TXT          = txt_sub,
            NULL)

        t_x[[i_x]] <- tsf_block_line_token(x_type, x_i, x_sub)
    }

    # Drop superfluous NULL token.
    return(t_x[-1L])
}

#' @rdname from-tsf
#' @keywords internal
tokenize_tsf_block_line_v1 <- function(x = character(1L)) {
    if (!nzchar(x)) {
        return("TXT")
    }

    i_type <- 1L

    # Looping is a bit faster than
    # the usual vectorized approach.
    while (!grepl(.TSF_SRC_BLOCK_LINE_TOKEN_PATTERNS[[i_type]], x)) {
        i_type <- i_type + 1L
    }

    return(names(.TSF_SRC_BLOCK_LINE_TOKEN_PATTERNS)[[i_type]])
}


# Internal constants -----------------------------------------------------------


.TSF_SRC_BLOCK_LINE_TOKEN_PATTERNS <- c(
    TITLE_HASH    = "^\\#[ \t]*`\\{\\{[ \t]*(.*?)\\}\\}`[ \t]*$",
    TITLE_KEY_SRC = "^\\#\\#[ \t]*`\\{\\{[ \t]*(.*?)\\}\\}`[ \t]*",
    TITLE_KEY_TXT = "^\\#\\#",
    LOC_SRC_PATH  = "^`(.*?)`:$",
    LOC_SRC_RNG   = "^[ \t]*-[ \t]*line[ \t]+[0-9]+,[ \t]*column[ \t]+[0-9]+[ \t]+@[ \t]+line[ \t]+[0-9]+,[ \t]*column[ \t]+[0-9]+$",
    TXT           = "")

.TSF_SRC_BLOCK_LOC_RNG <- paste("[ \t]*", "@", ",", "-[ \t]*line", "column", "line", sep = "|")
