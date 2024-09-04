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

    # Validate actual template_version.
    # Discard it afterwards so that it
    # is not treated as a custom field.
    assert_match(
        fields$template_version,
        get_template_versions(),
        x_name = "template_version")

    # If we ever have to support multiple templates,
    # we will choose a parser by calling switch() on
    # template_version value (extracted from fields).
    return(do.call(from_src_header_version_1, fields))
}

from_src_header_version_1 <- function(
    template_version = 1L,
    generated_by     = get_generated_by(),
    generated_on     = get_generated_on(),
    hash_algorithm   = get_hash_algorithms(),
    hash_length      = 32L,
    hashes           = character(),
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
    assert_int1(hash_length)

    hash_length_range <- get_hash_length_range(hash_algorithm)

    assert_between(
        hash_length,
        hash_length_range[["min"]],
        hash_length_range[["max"]])

    assert_chr(hashes, TRUE)
    assert_chr(language_keys, TRUE)
    assert_names(language_keys)

    if (!is_named(further_fields <- list(...))) {
        stops("all further fields (custom user's fields) must be named.")
    }
    if (length(hashes) && any(nchar(hashes) != hash_length)) {
        stops("all 'hashes' must have a length equal to 'hash_length'.")
    }

    return(
        list(
            template_version = 1L,
            generated_by     = generated_by,
            generated_on     = generated_on,
            hash_algorithm   = hash_algorithm,
            hash_length      = hash_length,
            hashes           = hashes,
            language_keys    = language_keys,
            further_fields   = further_fields))
}
