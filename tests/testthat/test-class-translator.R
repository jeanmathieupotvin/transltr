language_source_set("en")
withr::defer(language_source_set(NULL))

test_translator <- function() {
    return(
        translator(
            id = "test-translator",
            en = "English",
            fr = "Français",
            text(
                location("a", 1L, 2L, 3L, 4L),
                en = "Hello, world!",
                fr = "Bonjour, monde!"),
            text(
                location("b", 5L, 6L, 7L, 8L),
                en = "Farewell, world!",
                fr = "Au revoir, monde!")))
}

# We use tr1 (and tr2 if two objects are required) whenever
# the underlying test translator does not induce a permanent
# state change. Otherwise, a new object is generated within
# the test_that() block.
tr1 <- test_translator()
tr2 <- translator(
    id = "test-translator-2",
    en = "English",
    el = "Ελληνικά",
    text(
        location("c", 5L, 6L, 7L, 8L),
        en = "Hello, world!",
        el = "Γεια σου, Κόσμος!"))

# Extract private methods for testing purposes.
hash_compress   <- tr1$.__enclos_env__$private$.hash_reduce
hash_decompress <- tr1$.__enclos_env__$private$.hash_expand

# Class: active bindings -------------------------------------------------------

test_that("active binding id returns id", {
    expect_identical(tr1$id, "test-translator")
})

test_that("active binding id sets new value", {
    tr <- test_translator()
    tr$id <- "new-id"
    expect_identical(tr$id, "new-id")
})

test_that("active binding id validates value", {
    expect_error(tr1$id <- 1L)
    expect_snapshot(tr1$id <- 1L, error = TRUE)
})

test_that("active binding algorithm returns algorithm", {
    expect_identical(tr1$algorithm, "sha1")
})

test_that("active binding algorithm sets new value", {
    tr <- test_translator()
    tr$algorithm <- "utf8"

    expect_identical(tr$algorithm, "utf8")

    # Test that Texts objects' algorithm are updated.
    expect_identical(tr$hashes, c(`12351` = "12351", `17818` = "17818"))
    expect_identical(tr$get_text("12351")$hash, "12351")
    expect_identical(tr$get_text("17818")$hash, "17818")

    # Test that Text objects are reassigned to new hashes.
    expect_null(tr$get_text("256e0d7"))
    expect_null(tr$get_text("2ac373a"))
})

test_that("active binding algorithm validates value", {
    expect_error(tr1$algorithm <- 1L)
    expect_error(tr1$algorithm <- "new-algo")
    expect_snapshot(tr1$algorithm <- 1L,         error = TRUE)
    expect_snapshot(tr1$algorithm <- "new-algo", error = TRUE)
})

test_that("active binding hashes returns hashes", {
    expect_null(Translator$new()$hashes)
    expect_identical(tr1$hashes, c(
        `256e0d7` = "256e0d707386d0fcd9abf10ad994000bdaa25812",
        `2ac373a` = "2ac373aa699a6712cdaddbead28031d537de29bc"))
})

test_that("active binding hashes validates value", {
    expect_error(tr1$hashes <- "error")
    expect_snapshot(tr1$hashes <- "error", error = TRUE)
})

test_that("active binding source_texts returns source texts", {
    expect_null(Translator$new()$source_texts)
    expect_identical(tr1$source_texts, c(
        `256e0d7` = "Hello, world!",
        `2ac373a` = "Farewell, world!"))
})

test_that("active binding source_texts validates value", {
    expect_error(tr1$source_texts <- 1L)
    expect_snapshot(tr1$source_texts <- 1L, error = TRUE)
})

test_that("active binding source_langs returns source languages", {
    tr <- translator(
        en = "English",
        fr = "Français",
        text(
            en = "Hello, world!",
            fr = "Bonjour, monde!",
            source_lang = "en"),
        text(
            en = "Farewell, world!",
            fr = "Au revoir, monde!",
            source_lang = "fr"))

    expect_null(Translator$new()$source_langs)
    expect_identical(tr1$source_langs, "en")
    expect_identical(tr$source_langs, c(`256e0d7` = "en", f3ae57a = "fr"))
})

test_that("active binding source_langs validates value", {
    expect_error(tr1$source_langs <- "new-source-lang")
    expect_snapshot(tr1$source_langs <- "new-source-lang", error = TRUE)
})

test_that("active binding languages returns languages", {
    # This implicitly checks that hashes are also sorted.
    expect_null(Translator$new()$languages)
    expect_identical(tr1$languages, c("en", "fr"))
})

test_that("active binding languages drops duplicated languages", {
    tr <- test_translator()
    tr$set_text(en = "Hello, world!", el = "Γεια σου, Κόσμος!")
    expect_identical(tr$languages, c("el", "en", "fr"))
})

test_that("active binding languages validates value", {
    expect_error(tr1$languages <- "error")
    expect_snapshot(tr1$languages <- "error", error = TRUE)
})

test_that("active binding native_languages returns native languages", {
    # This implicitly checks that hashes are also sorted.
    expect_null(Translator$new()$native_languages)
    expect_identical(tr1$native_languages, c(en = "English", fr = "Français"))
})

test_that("active binding native_languages validates value", {
    expect_error(tr1$native_languages <- "error")
    expect_snapshot(tr1$native_languages <- "error", error = TRUE)
})

# Class: private methods -------------------------------------------------------

test_that("$.hash_reduce() works", {
    hashes <- c("256e0d707386d0", "2ac373aa699a67")

    expect_identical(
        hash_compress(hashes),
        c(`256e0d707386d0` = "256e0d7", `2ac373aa699a67` = "2ac373a"))
    expect_identical(
        hash_compress(c("short", "hashes")),
        c(short = "short", hashes = "hashes"))
})

test_that("$.hash_expand() works", {
    expect_identical(hash_decompress("256e0d7"), "256e0d707386d0fcd9abf10ad994000bdaa25812")
    expect_identical(hash_decompress("2ac373a"), "2ac373aa699a6712cdaddbead28031d537de29bc")

})

test_that("$.hash_expand() returns <notfound> if hash has no match", {
    expect_identical(hash_decompress("test-hash"), "<notfound>")
})

# Class: public methods --------------------------------------------------------

test_that("$initialize() works", {
    # It can only be tested indirectly via $new().
    tr <- Translator$new("test-id", "utf8")

    expect_s3_class(tr, c("Translator", "R6"), exact = TRUE)
    expect_identical(tr$id, "test-id")
    expect_identical(tr$algorithm, "utf8")
    expect_type(tr$.__enclos_env__$private$.native_langs, "environment")
    expect_type(tr$.__enclos_env__$private$.texts, "environment")
})

test_that("$translate() returns a character string if translation is available", {
    # By design, this also checks that inputs are hashed accordingly.
    expect_identical(tr1$translate("Hello, world!",    lang = "en"), "Hello, world!")
    expect_identical(tr1$translate("Hello, world!",    lang = "fr"), "Bonjour, monde!")
    expect_identical(tr2$translate("Hello, world!",    lang = "el"), "Γεια σου, Κόσμος!")
    expect_identical(tr1$translate("Farewell, world!", lang = "en"), "Farewell, world!")
    expect_identical(tr1$translate("Farewell, world!", lang = "fr"), "Au revoir, monde!")
})

test_that("$translate() returns null if translation is not available", {
    expect_null(tr1$translate("Hello, world!", lang = "el"))
})

test_that("$translate() validates source_lang", {
    expect_error(tr1$translate(lang = "en", source_lang = 1L))
    expect_snapshot(tr1$translate(lang = "en", source_lang = 1L), error = TRUE)
})

test_that("$get_translation() works", {
    expect_null(tr1$get_translation("bad-hash", "en"))
    expect_null(tr1$get_translation(hash("en", "bad-hash", "sha1"), "en"))
    expect_identical(tr1$get_translation("2ac373a", "en"), "Farewell, world!")
    expect_identical(tr1$get_translation("2ac373a", "fr"), "Au revoir, monde!")
    expect_identical(tr1$get_translation("256e0d7", "en"), "Hello, world!")
    expect_identical(tr1$get_translation("256e0d7", "fr"), "Bonjour, monde!")
})

test_that("$get_text() works", {
    expect_null(tr1$get_text(hash("en", "bad-hash", "sha1")))
    expect_identical(tr1$get_text("2ac373a")$hash, "2ac373aa699a6712cdaddbead28031d537de29bc")
    expect_identical(tr1$get_text("256e0d7")$hash, "256e0d707386d0fcd9abf10ad994000bdaa25812")
})

test_that("$get_text() validates hash", {
    expect_error(tr1$get_text(hash = 1L))
    expect_snapshot(tr1$get_text(hash = 1L), error = TRUE)
})

test_that("$set_text() returns null invisibly", {
    expect_null(test_translator()$set_text(en = "Bye bye!"))
    expect_invisible(test_translator()$set_text(en = "Bye bye!"))
})

test_that("$set_text() creates and registers a Text object", {
    tr <- test_translator()
    tr$set_text(en = "Bye bye!", location("a"))
    txt <- tr$get_text(hash("en", "Bye bye!", "sha1"))

    expect_s3_class(txt, "Text")
    expect_identical(txt$source_lang, "en")
    expect_identical(txt$source_text, "Bye bye!")
    expect_identical(txt$locations, list(a = location("a")))
})

test_that("$set_texts() returns null invisibly", {
    tr   <- Translator$new()
    txt1 <- text(en = "Bye bye!", location("a"))
    txt2 <- text(fr = "À la prochaine!", location("b"), source_lang = "fr")

    # Case ... is empty.
    expect_null(tr$set_texts())
    expect_invisible(tr$set_texts())

    # Case ... is not empty.
    expect_null(tr$set_texts(txt1, txt2))
    expect_invisible(tr$set_texts(txt1, txt2))
})

test_that("$set_texts() registers Text objects", {
    tr   <- Translator$new()
    txt1 <- text(en = "Bye bye!", location("a"))
    txt2 <- text(fr = "À la prochaine!", location("b"), source_lang = "fr")
    tr$set_texts(txt1, txt2)

    expect_identical(tr$get_text(hash("en", "Bye bye!", "sha1")),        txt1)
    expect_identical(tr$get_text(hash("fr", "À la prochaine!", "sha1")), txt2)
})

test_that("$set_texts() validates ...", {
    expect_error(Translator$new()$set_texts(1L, text(en = "Bye bye!")))
    expect_snapshot(error = TRUE, {
        Translator$new()$set_texts(1L, text(en = "Bye bye!"))
    })
})

test_that("$rm_text() returns null invisibly", {
    tr <- test_translator()
    expect_null(tr$rm_text("256e0d7"))
    expect_invisible(tr$rm_text("2ac373a"))
})

test_that("$rm_text() throws an error if there are no Text objects to remove", {
    expect_error(Translator$new()$rm_text("error"))
    expect_snapshot(Translator$new()$rm_text("error"), error = TRUE)
})

test_that("$rm_text() validates hash", {
    expect_error(tr1$rm_text(1L))
    expect_error(tr1$rm_text("error"))
    expect_snapshot(tr1$rm_text(1L),      error = TRUE)
    expect_snapshot(tr1$rm_text("error"), error = TRUE)
})

test_that("$rm_text() removes Text objects as expected", {
    tr <- test_translator()
    tr$rm_text("256e0d7")
    expect_length(tr$hashes, 1L)
    expect_identical(tr$hashes, c(`2ac373a` = "2ac373aa699a6712cdaddbead28031d537de29bc"))
})

test_that("$set_native_languages() returns null invisibly", {
    tr <- Translator$new()

    # Case ... is empty.
    expect_null(tr$set_native_languages())
    expect_invisible(tr$set_native_languages())

    # Case ... is not empty.
    expect_null(tr$set_native_languages(en = "English"))
    expect_invisible(tr$set_native_languages(en = "English"))
})

test_that("$set_native_languages() registers native languages", {
    tr <- Translator$new()
    tr$set_native_languages(
        en = "English",
        # Test whether duplicate keys work and
        # silently overwrites previous values.
        en = "English-dup",
        fr = "Français",
        ja = "日本語")

    expect_identical(
        tr$native_languages,
        c(en = "English-dup", fr = "Français", ja = "日本語"))
})

test_that("$set_native_languages() removes native languages", {
    tr <- test_translator()
    tr$set_native_languages(fr = NULL)
    expect_identical(tr$native_languages, c(en = "English"))
})

test_that("$set_native_languages() validates ...", {
    expect_error(tr1$set_native_languages(1L))
    expect_error(tr1$set_native_languages("English"))
    expect_snapshot(tr1$set_native_languages(1L), error = TRUE)
    expect_snapshot(tr1$set_native_languages("English"), error = TRUE)
})

test_that("$set_default_value() returns null invisibly", {
    tr <- Translator$new()

    expect_null(tr$set_default_value())
    expect_invisible(tr$set_default_value())
})

test_that("$set_default_value() validates value if not null", {
    tr <- Translator$new()

    expect_error(tr$set_default_value(1L))
    expect_snapshot(tr$set_default_value(1L), error = TRUE)
})

test_that("$set_default_value() registers default value", {
    tr <- Translator$new()
    tr$set_default_value("test-value")

    expect_identical(tr$translate("Unavailable text."),       "test-value")
    expect_identical(tr$get_translation("Unavailable text."), "test-value")
})

# translator() -----------------------------------------------------------------

test_that("translator() returns an R6 object of class Translator", {
    tr <- translator(
        id = "test-translator",
        en = "English",
        fr = "Français",
        text(
            location("a", 1L, 2L, 3L, 4L),
            en = "Hello, world!",
            fr = "Bonjour, monde!"),
        algorithm = "utf8",
        # These arguments should be ignored silently.
        1L, 1.0, 1.0 + 2i, raw(1L))

    expect_s3_class(tr, c("Translator", "R6"), exact = TRUE)
    expect_identical(tr$id, "test-translator")
    expect_identical(tr$algorithm, "utf8")
    expect_identical(tr$hashes, c(`12351` = "12351"))
    expect_identical(tr$source_texts, c(`12351` = "Hello, world!"))
    expect_identical(tr$languages, c("en", "fr"))
    expect_identical(tr$native_languages, c(en = "English", fr = "Français"))
})

test_that("translator() throws a warning if a language does not have a corresponding native language", {
    expect_warning(
        translator(
            id = "test-translator",
            en = "English",
            text(en = "Hello, world!", fr = "Bonjour, monde!")))
    expect_snapshot(
        translator(
            id = "test-translator",
            en = "English",
            text(en = "Hello, world!", fr = "Bonjour, monde!")))
})

# is_translator() --------------------------------------------------------------

test_that("is_translator() works", {
    expect_true(is_translator(Translator$new()))
    expect_false(is_translator(1L))
})

# format.Translator() ----------------------------------------------------------

test_that("format() returns a character", {
    # This test block is a little bit
    # fragile, but hardcoding expected
    # values is simpler.
    fmt_tr1      <- format(tr1)
    fmt_tr_empty <- format(Translator$new(id = "test-translator"))

    expect_type(fmt_tr1, "character")
    expect_length(fmt_tr1, 9L)
    expect_identical(fmt_tr1, c(
        "<Translator>",
        " Identifier: test-translator",
        " Algorithm: sha1",
        " Languages:",
        "  en: English",
        "  fr: Français",
        " Source Text:",
        "  256e0d7 [en, fr]: Hello, world!",
        "  2ac373a [en, fr]: Farewell, world!"))

    expect_type(fmt_tr_empty, "character")
    expect_length(fmt_tr_empty, 5L)
    expect_identical(fmt_tr_empty, c(
        "<Translator>",
        " Identifier: test-translator",
        " Algorithm: sha1",
        " Languages: <null>",
        " Source Text: <null>"))
})

test_that("format() escapes newlines", {
    tr <- Translator$new(id = "test-translator")
    tr$set_text(en = "Hello,\n\nworld!")
    out <- format(tr)

    expect_match(out[[6L]], r"{\\n\\n}")
    expect_snapshot(print(tr))
})

# print.Translator() -----------------------------------------------------------

test_that("print() works", {
    expect_output(print(tr1))
    expect_snapshot(print(tr1))
})

test_that("print() returns x invisibly", {
    withr::local_output_sink(tempfile())
    expect_invisible(print(tr1))
    expect_identical(print(tr1), tr1)
})
