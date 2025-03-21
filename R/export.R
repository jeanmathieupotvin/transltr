#' @rdname serialize
#' @keywords internal
export <- function(x, ...) {
    UseMethod("export")
}

#' @rdname serialize
#' @keywords internal
#' @export
export.Translator <- function(x, ...) {
    out <- x$as_list(FALSE)

    # Lists are required to preserve names in
    # the resulting serialized YAML string.
    out$language_names <- as.list(out$language_names)
    out$source_texts <- unname(lapply(out$source_texts, export, ...))

    return(
        structure(out,
            tag   = "Translator",
            class = c("ExportedTranslator", "Exported")))
}

#' @rdname serialize
#' @keywords internal
#' @export
export.SourceText <- function(x, set_translations = TRUE, ...) {
    assert_lgl1(set_translations)

    out <- x$as_list(FALSE, TRUE)

    # Widths are set so that no line will
    # ever be longer than 80 characters.
    # TODO: Review magic widths values.
    out$source_text <- str_wrap(out$source_text, width = 74L)
    out$translations <- if (set_translations) {
        lapply(out$translations, str_wrap, width = 72L)
    }

    out$source_locations <- unname(lapply(out$source_locations, export, ...))

    return(
        structure(out,
            tag   = "SourceText",
            class = c("ExportedSourceText", "SourceText")))
}

#' @rdname serialize
#' @keywords internal
#' @export
export.SourceLocation <- function(x, ...) {
    return(
        structure(as.list(x),
            tag   = "SourceLocation",
            class = c("ExportedSourceLocation", "SourceLocation")))
}

#' @rdname serialize
#' @export
assert.ExportedTranslator <- function(
    x,
    error = ErrorReporter$new(),
    throw = TRUE,
    ...)
{
    # This prevents any out-of-bound
    # errors that may stem from `[[`.
    if (!is.list(x)) {
        x <- list()
    }

    # Accumulate error messages.
    errors <- c(
        if (!is_chr1(value)) {
            stops("'$id' must be 'NULL', or a non-NA and non-empty character of length 1.")
        },
        if (!is.null(value) && !is_chr1(value)) {
            stops("'$current_lang' must be 'NULL', or a non-NA and non-empty character of length 1.")
        },
        if (!is.null(value) && !is_chr1(value, TRUE)) {
            stops("'$default_value' must be 'NULL', or a non-NA character of length 1.")
        },
        if (!is_list(langs, TRUE) ||
            !is_named(langs) ||
            !all(vapply_1l(langs, is_chr1))) {
            "'Languages' must a mapping of non-empty character strings."
        },
        # Validate Texts.
        if (!is_list(texts, TRUE) ||
            !all(vapply_1l(texts, inherits, what = "ExportedText"))) {
            "'Texts' must a sequence of 'Text' objects."
        },
        # Validate contents of each Text object.
        unlist(lapply(texts, assert, throw = FALSE, ...))
    )

    if (length(errors)) {
        return(report_errors(errors, id, throw))
    }

    return(character())
}

#' @rdname serialize
#' @export
assert.ExportedSourceText <- function(
    x,
    reporter  = error_reporter(),
    throw     = TRUE,
    id_algo   = "xxh32",
    hash_algo = "xxh32",
    ...)
{
    if (!is_error_reporter(reporter)) {
        stops("'reporter' must be an 'ErrorReporter' object.")
    }

    x <- make_safe(x)

    # Link errors pushed below to the
    # object underlying (safe) identifier.
    reporter$error_label <- attr(x, "_id")

    if (!is_hash(x$id, id_algo)) {
        reporter$push(
            sprintf(
                "'id' must be a non-empty character string of %i hexadecimal characters.",
                hash_std_template(id_algo)[["size"]]
            )
        )
    }

    if (!is_chr1(x$source_lang)) {
        reporter$push("'source_lang' must be a non-empty character string.")
    }

    if (!is_chr1(x$source_text)) {
        reporter$push("'source_text' must be a character string. It can be empty.")
    }

    if (!is_hash(x$source_text_hash, hash_algo)) {
        reporter$push(
            sprintf(
                "'source_text_hash' must be a non-empty character string of %i hexadecimal characters.",
                hash_std_template(hash_algo)[["size"]]
            )
        )
    }

    if (!is_list(x$source_locations, TRUE) || !all(is_source_locations)) {
        reporter$push("'source_locations' must be a sequence of 'Location' objects. It can be empty.")
    }

    # TODO: error checks on source_locations.
    # TODO: error checks on translations.
    unlist(lapply(x$source_locations, assert, throw = FALSE, ...))

    is_source_locations <- vapply_1l(
        x$source_locations,
        inherits,
        what = "ExportedSourceLocation")

    return(reporter$report(throw))
}

#' @rdname serialize
#' @export
assert.ExportedSourceLocation <- function(
    x,
    reporter = error_reporter(),
    throw    = TRUE,
    id_algo  = "xxh32",
    ...)
{
    if (!is_error_reporter(reporter)) {
        stops("'reporter' must be an 'ErrorReporter' object.")
    }

    x <- make_safe(x)

    # Link errors pushed below to the
    # object underlying (safe) identifier.
    reporter$error_label <- attr(x, "_id")

    if (!is_hash(x$id, id_algo)) {
        reporter$push(
            sprintf(
                "'id' must be a non-empty character string of %i hexadecimal characters.",
                hash_std_template(id_algo)[["size"]]
            )
        )
    }

    if (!is_chr1(x$path)) {
        reporter$push("'path' must be a non-empty character string.")
    }

    if (!is_chr(x$ranges) || !all(range_is_parseable(x$ranges))) {
        reporter$push(
            sprintf(
                "'ranges' must be a single %s character string, or a sequence of such values.",
                range_std_template("user")
            )
        )
    }

    return(reporter$report(throw))
}

make_safe <- function(x, default_id = "<unknown>", ...) {
    assert_chr1(default_id)

    # Ensure that if x is incorrectly labelled as
    # an Exported* object, no error is thrown when
    # extracting missing fields with $. Errors can
    # be accumulated and later reported.
    if (!is.list(x)) {
        x <- structure(as.list(x), class = class(x))
    }

    attr(x, "_id") <- if (is.null(x$id)) default_id else as.character(x$id[[1L]])
    return(x)
}
