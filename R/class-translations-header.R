translations_header <- function(
    .template_version = 1L,
    .generated_by     = "R package transltr 0.0.1",
    .generated_on     = utc(),
    .hash_algorithm   = "blake2b",
    .hash_length      = 32L,
    .hashes           = character(),
    language_keys     = character(),
    ...)
{
    # Errors are accumulated and thrown at the same
    # time with one call to stops() for convenience.
    err_msgs <- c(
        assert_choice(.template_version,     throw_error = FALSE),
        assert_int1(.template_version,       throw_error = FALSE),
        assert_chr1(.generated_by,           throw_error = FALSE),
        assert_chr1(.generated_on,           throw_error = FALSE),
        assert_choice(.hash_algorithm, TRUE, throw_error = FALSE),
        assert_int1(.hash_length,            throw_error = FALSE),
        assert_chr(.hashes, TRUE,            throw_error = FALSE),
        assert_chr(language_keys, TRUE,      throw_error = FALSE),
        assert_names(language_keys,          throw_error = FALSE),
        if (!is_named(list(...))) {
            "All custom fields must be named."
        },
        if (length(.hashes) && any(nchar(.hashes) != .hash_length)) {
            "All '.hashes' must have a length equal to '.hash_length'."
        }
    )

    # Keep non-empty error messages and throw
    # an error if there is at least one.
    if (length(err_msgs <- err_msgs[nzchar(err_msgs)])) {
        stops(
            "source translation file's header is invalid. See errors below.\n",
            sprintf(" [%i] %s\n", seq_along(err_msgs), err_msgs))
    }

    return(
        structure(
            list(
                .template_version = .template_version,
                .generated_by     = .generated_by,
                .generated_on     = .generated_on,
                .hash_algorithm   = .hash_algorithm,
                .hash_length      = .hash_length,
                .hashes           = .hashes,
                language_keys     = language_keys,
                ...),
            class = c("TranslationsHeader", "list")))
}

is_translation_header <- function(x) {
    return(inherits(x, "TranslationsHeader"))
}
