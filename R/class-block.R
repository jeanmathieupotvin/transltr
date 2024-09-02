new_translation <- function(lang = "", text = "") {
    assert_chr1(lang)
    assert_chr1(text)

    names(text) <- lang
    return(text)
}

new_block <- function(id = "", translations = character()) {
    assert_chr1(id)
    assert_chr(translations)
    assert_names(translations)

    return(
        structure(
            list(id = id, translations = translations),
            class = c("Block", "list")))
}

is_block <- function(x) {
    return(inherits(x, "Block"))
}
