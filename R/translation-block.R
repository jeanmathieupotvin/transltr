md_file  <- readLines(file.path(".local", "mocks-temp", "mock.transltr.md"))
md_block <- md_file[40L:42L]

new_transltr_block <- function(
    id = "",
    metadata = list(locations = character()),
    source = list(),
    translations = list()
) {

}

new_translation <- function(lang = "", text = "") {
    assert_non_empty_string(lang)
    assert_non_empty_string(text)
    return(structure(list(lang = lang, text = text), class = "Translation"))
}

parse_md_translation <- function(md_block = character()) {

    lang <- gsub("^#{3}[ \t]*", "", md_block[[1L]])
}
