new_translation <- function(lang = "", text = "") {
    assert_chr1(lang)
    assert_chr1(text)

    return(
        structure(
            list(lang = lang, text = text),
            class = c("Translation", "list")))
}

new_translation_block <- function(id = "", translations = list()) {
    assert_chr1(id)
    assert_list(translations)

    if (!all(vapply_1l(translations, is_translation))) {
        stops("'translations' can only contain proper 'Translation' objects.")
    }

    # Assign names for faster extractions later.
    names(translations) <- vapply_1c(translations, `[[`, i = "lang")

    return(
        structure(
            list(id = id, translations = translations),
            class = c("TranslationsBlock", "list")))
}

is_translation <- function(x) {
    return(inherits(x, "Translation"))
}

is_translations_block <- function(x) {
    return(inherits(x, "TranslationsBlock"))
}
