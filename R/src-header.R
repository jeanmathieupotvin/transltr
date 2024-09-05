#' Extract, parse, and validate source headers
#'
#' Extract headers from source Markdown files that contain translations
#' formatted according to what [`transltr`][transltr] prescribes. Parse
#' their contents and validate it.
#'
#' @details
#' Package [`transltr`][transltr] stores translations and relevent metadata
#' into a single plain Markdown file. As such, it follows the usual syntax
#' of [CommonMark](https://commonmark.org/) with one exception: it requires
#' a [YAML](https://yaml.org/) header. While it is strongly recommended to
#' keep that header at the very top of the file, it can be anywhere within it.
#'
#' ## General structure
#'
#' Source Markdown files are divided into two parts: the **header** and the
#' actual **translations**. Its general structure is as follows.
#'
#' ```plain
#' # File start
#' ---
#' Header
#' ---
#' # Empty line(s)
#' Translations
#' # File end
#' ```
#'
#' The YAML header must always be enclosed by a pair of separators (three
#' consecutive dashes) located on separate lines. In-line headers are
#' disallowed for better readability.
#'
#' ## Template versions
#'
#' Since the format may change in the future, multiple ones may need to be
#' supported on a long-term basis. Therefore, the header always requires a
#' `template_version` field that specifies required header fields and how
#' translations are structured.
#'
#' ## Template version 1
#'
#' This version enforces `7` mandatory header fields. As long as they have
#' keys (names), users may add any further fields to this list. A valid example
#' is provided below.
#'
#' ```yaml
#' template_version: 1
#' generated_by: R package transltr 0.0.1
#' generated_on: August 22, 2024 @ 08:00 UTC
#' hash_algorithm: blake2b
#' hash_length: 32
#' language_keys:
#'     en: English
#'     fr: Français
#'     es: Español
#'     jp: 日本語
#' ```
#'
#' Further fields may appear anywhere within the header (there is no preferred
#' order).
#'
#' @param x A character vector of source lines.
#'
#' @param ... Further custom fields. They must always be named.
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
#' [extract_src_header()] returns a character vector representing source
#' YAML lines ready to be parsed as an \R list by [from_src_header()].
#'
#' [from_src_header()] returns a named list. Its contents depends on the
#' underlying (parsed) `template_version` value.
#'
#' [from_src_header_version_1()] returns a named list of length 7 that
#' contains parsed (and valid) values of its arguments. It also returns
#' a `further_fields` element which is a named list constructed from `...`.
#'
#' @note
#' Note that [from_src_header_version_1()] assumes `template_version` is
#' always equal to `1` and ignores any value passed to it. The argument is
#' included to ease the implementation of [from_src_header()]. The latter
#' is in charge of validating it.
#'
# TODO: Update @seealso with further mechanisms when they are implemented.
#' @seealso [extract_src_blocks()], [from_src_block()]
#'
#' @rdname src-header
#' @family source header mechanisms
#' @keywords internal
extract_src_header <- function(x = character()) {
    # We process the first pair of
    # separators and ignore the rest.
    sep_pos  <- grep("^---$", x)[c(1L, 2L)]
    na_count <- sum(is.na(sep_pos))
    indices  <- switch(na_count + 1L,
        # Case 1: na_count = 0: there is a header.
        seq.int(sep_pos[[1L]], sep_pos[[2L]]),
        # Case 2: na_count = 1: missing separator is treated as a format error.
        stops(
            "header's format is invalid. It misses a separator ('---').\n",
            "Each separator must be on its own line to be detected."),
        # Case 3: na_count = 2: there is no header at all.
        stops("a header is always required. Regenerate the underlying file."))

    # A non-empty header must span at least 3
    # lines. Else, it must be empty by design.
    n_indices <- length(indices)
    return(if (n_indices > 2L) x[indices[-c(1L, n_indices)]] else "")
}

#' @rdname src-header
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
            do.call(from_src_header_version_1, fields)))
}

#' @usage
#' ## Called internally by from_src_header()
#' from_src_header_version_1(
#'   template_version = 1L,
#'   generated_by     = get_generated_by(),
#'   generated_on     = get_generated_on(),
#'   hash_algorithm   = get_hash_algorithms(),
#'   hash_length      = 32L,
#'   language_keys    = list(en = "English")
#' )
#'
#' @rdname src-header
#' @keywords internal
from_src_header_version_1 <- function(
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
    assert_names(language_keys)
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
