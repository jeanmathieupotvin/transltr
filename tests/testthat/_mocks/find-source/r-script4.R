# This script mocks a real R script containing a custom interface function
# calling method Translator$translate(). It must be ignored by all tests of
# find_source_*() functions.

translate <- function(..., lang = language_get()) {
    return(tr$translate(..., lang = lang, source_lang = "en"))
}
