language_source_set("en")
withr::defer(language_source_set(NULL))

test_translator <- function() {
    return(
        translator(
            id = "test-translator",
            en = "English",
            fr = "Français",
            block(
                location("a", 1L, 2L, 3L, 4L),
                en = "Hello, world!",
                fr = "Bonjour, monde!"),
            block(
                location("b", 5L, 6L, 7L, 8L),
                en = "Farewell, world!",
                fr = "Au revoir, monde!")))
}

# We use trans1 (and trans2 if two objects are required)
# whenever the underlying test translator does not induce
# a permanent state change. Otherwise, a new object is
# generated within the test_that() expression.
trans1 <- test_translator()
trans2 <- translator(
    id = "test-translator-2",
    en = "English",
    el = "Ελληνικά",
    block(
        location("c", 5L, 6L, 7L, 8L),
        en = "Hello, world!",
        el = "Γεια σου, Κόσμος!"))

# Extract private methods for testing purposes.
hash_compress   <- trans1$.__enclos_env__$private$.hash_reduce
hash_decompress <- trans1$.__enclos_env__$private$.hash_expand


# Class: active bindings -------------------------------------------------------


test_that("active binding id returns id", {
    expect_identical(trans1$id, "test-translator")
})

test_that("active binding id sets new value", {
    trans <- test_translator()
    trans$id <- "new-id"
    expect_identical(trans$id, "new-id")
})

test_that("active binding id validates value", {
    expect_error(trans1$id <- 1L)
    expect_snapshot(trans1$id <- 1L, error = TRUE)
})

test_that("active binding hash_algorithm returns algorithm", {
    expect_identical(trans1$hash_algorithm, "sha1")
})

test_that("active binding hash_algorithm sets new value", {
    trans <- test_translator()
    trans$hash_algorithm <- "utf8"

    expect_identical(trans$hash_algorithm, "utf8")

    # Test that Blocks objects' hash_algorithm are updated.
    expect_identical(trans$hashes, c("12351", "17818"))
    expect_identical(trans$get_block("12351")$hash, "12351")
    expect_identical(trans$get_block("17818")$hash, "17818")

    # Test that Block objects are reassigned to new hashes.
    expect_null(trans$get_block("256e0d7"))
    expect_null(trans$get_block("2ac373a"))
})

test_that("active binding hash_algorithm validates value", {
    expect_error(trans1$hash_algorithm <- 1L)
    expect_error(trans1$hash_algorithm <- "new-algo")
    expect_snapshot(trans1$hash_algorithm <- 1L,         error = TRUE)
    expect_snapshot(trans1$hash_algorithm <- "new-algo", error = TRUE)
})

test_that("active binding hashes returns hashes", {
    expect_null(Translator$new()$hashes)
    expect_identical(trans1$hashes, c(
        "256e0d707386d0fcd9abf10ad994000bdaa25812",
        "2ac373aa699a6712cdaddbead28031d537de29bc"))
})

test_that("active binding hashes validates value", {
    expect_error(trans1$hashes <- "error")
    expect_snapshot(trans1$hashes <- "error", error = TRUE)
})

test_that("active binding source_texts returns source texts", {
    expect_null(Translator$new()$source_texts)
    expect_identical(trans1$source_texts, c(
        `256e0d7` = "Hello, world!",
        `2ac373a` = "Farewell, world!"))
})

test_that("active binding source_texts validates value", {
    expect_error(trans1$source_texts <- 1L)
    expect_snapshot(trans1$source_texts <- 1L, error = TRUE)
})

test_that("active binding languages returns languages", {
    # This implicitly checks that hashes are also sorted.
    expect_null(Translator$new()$languages)
    expect_identical(trans1$languages, c("en", "fr"))
})

test_that("active binding languages drops duplicated languages", {
    trans <- test_translator()
    trans$set_block(en = "Hello, world!", el = "Γεια σου, Κόσμος!")
    expect_identical(trans$languages, c("el", "en", "fr"))
})

test_that("active binding languages validates value", {
    expect_error(trans1$languages <- "error")
    expect_snapshot(trans1$languages <- "error", error = TRUE)
})

test_that("active binding native_languages returns native languages", {
    # This implicitly checks that hashes are also sorted.
    expect_null(Translator$new()$native_languages)
    expect_identical(trans1$native_languages, c(en = "English", fr = "Français"))
})

test_that("active binding native_languages validates value", {
    expect_error(trans1$native_languages <- "error")
    expect_snapshot(trans1$native_languages <- "error", error = TRUE)
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
    trans <- Translator$new("test-id", "utf8")

    expect_s3_class(trans, c("Translator", "R6"), exact = TRUE)
    expect_identical(trans$id, "test-id")
    expect_identical(trans$hash_algorithm, "utf8")
    expect_type(trans$.__enclos_env__$private$.native_langs, "environment")
    expect_type(trans$.__enclos_env__$private$.blocks, "environment")
})

test_that("$initialize() validates id", {
    expect_error(Translator$new(1L))
    expect_snapshot(Translator$new(1L), error = TRUE)
})

test_that("$initialize() validates hash_algorithm", {
    expect_error(Translator$new(hash_algorithm = 1L))
    expect_snapshot(Translator$new(hash_algorithm = 1L), error = TRUE)
})

test_that("$translate() returns a character string if translation is available", {
    # By design, this also checks that inputs
    # are hashed accordingly by text_hash().
    expect_identical(trans1$translate("Hello, world!",    lang = "en"), "Hello, world!")
    expect_identical(trans1$translate("Hello, world!",    lang = "fr"), "Bonjour, monde!")
    expect_identical(trans2$translate("Hello, world!",    lang = "el"), "Γεια σου, Κόσμος!")
    expect_identical(trans1$translate("Farewell, world!", lang = "en"), "Farewell, world!")
    expect_identical(trans1$translate("Farewell, world!", lang = "fr"), "Au revoir, monde!")
})

test_that("$translate() returns null if translation is not available", {
    expect_null(trans1$translate("Hello, world!", lang = "el"))
})

test_that("$translate() normalizes text", {
    # We only check that text_normalize() is called.
    # "Hello," and "world!" should be concatenated,
    # and translate() should return source text.
    expect_identical(trans1$translate("Hello,", "world!", lang = "en"), "Hello, world!")
})

test_that("$translate() validates lang", {
    expect_error(trans1$translate(lang = 1L))
    expect_snapshot(trans1$translate(lang = 1L), error = TRUE)
})

test_that("$translate() validates concat", {
    expect_error(trans1$translate(lang = "en", concat = 1L))
    expect_snapshot(trans1$translate(lang = "en", concat = 1L), error = TRUE)
})

test_that("$translate() validates source_lang", {
    expect_error(trans1$translate(lang = "en", source_lang = 1L))
    expect_snapshot(trans1$translate(lang = "en", source_lang = 1L), error = TRUE)
})

test_that("$get_translation() works", {
    expect_null(trans1$get_translation("bad-hash", "en"))
    expect_null(trans1$get_translation(text_hash("en", "bad-hash", "sha1"), "en"))
    expect_identical(trans1$get_translation("2ac373a", "en"), "Farewell, world!")
    expect_identical(trans1$get_translation("2ac373a", "fr"), "Au revoir, monde!")
    expect_identical(trans1$get_translation("256e0d7", "en"), "Hello, world!")
    expect_identical(trans1$get_translation("256e0d7", "fr"), "Bonjour, monde!")
})

test_that("$get_block() works", {
    expect_null(trans1$get_block(text_hash("en", "bad-hash", "sha1")))
    expect_identical(trans1$get_block("2ac373a")$hash, "2ac373aa699a6712cdaddbead28031d537de29bc")
    expect_identical(trans1$get_block("256e0d7")$hash, "256e0d707386d0fcd9abf10ad994000bdaa25812")
})

test_that("$get_block() validates hash", {
    expect_error(trans1$get_block(hash = 1L))
    expect_snapshot(trans1$get_block(hash = 1L), error = TRUE)
})

test_that("$set_block() returns null invisibly", {
    expect_null(test_translator()$set_block(en = "Bye bye!"))
    expect_invisible(test_translator()$set_block(en = "Bye bye!"))
})

test_that("$set_block() creates and registers a Block object", {
    trans <- test_translator()
    trans$set_block(en = "Bye bye!", location("a"))
    blk <- trans$get_block(text_hash("en", "Bye bye!", "sha1"))

    expect_s3_class(blk, "Block")
    expect_identical(blk$source_lang, "en")
    expect_identical(blk$source_text, "Bye bye!")
    expect_identical(blk$locations, list(a = location("a")))
})

test_that("$set_blocks() returns null invisibly", {
    trans <- Translator$new()
    blk1  <- block(en = "Bye bye!", location("a"))
    blk2  <- block(fr = "À la prochaine!", location("b"), source_lang = "fr")

    # Case ... is empty.
    expect_null(trans$set_blocks())
    expect_invisible(trans$set_blocks())

    # Case ... is not empty.
    expect_null(trans$set_blocks(blk1, blk2))
    expect_invisible(trans$set_blocks(blk1, blk2))
})

test_that("$set_blocks() registers Block objects", {
    trans <- Translator$new()
    blk1  <- block(en = "Bye bye!", location("a"))
    blk2  <- block(fr = "À la prochaine!", location("b"), source_lang = "fr")
    trans$set_blocks(blk1, blk2)

    expect_identical(trans$get_block(text_hash("en", "Bye bye!", "sha1")),        blk1)
    expect_identical(trans$get_block(text_hash("fr", "À la prochaine!", "sha1")), blk2)
})

test_that("$set_blocks() validates ...", {
    expect_error(Translator$new()$set_blocks(1L, block(en = "Bye bye!")))
    expect_snapshot(error = TRUE, {
        Translator$new()$set_blocks(1L, block(en = "Bye bye!"))
    })
})

test_that("$set_native_languages() returns null invisibly", {
    trans <- Translator$new()

    # Case ... is empty.
    expect_null(trans$set_native_languages())
    expect_invisible(trans$set_native_languages())

    # Case ... is not empty.
    expect_null(trans$set_native_languages(en = "English"))
    expect_invisible(trans$set_native_languages(en = "English"))
})

test_that("$set_native_languages() registers native languages", {
    trans <- Translator$new()
    trans$set_native_languages(
        en = "English",
        # Test whether duplicate keys work and
        # silently overwrites previous values.
        en = "English-dup",
        fr = "Français",
        ja = "日本語")

    expect_identical(
        trans$native_languages,
        c(en = "English-dup", fr = "Français", ja = "日本語"))
})

test_that("$set_native_languages() removes native languages", {
    trans <- test_translator()
    trans$set_native_languages(fr = NULL)
    expect_identical(trans$native_languages, c(en = "English"))
})

test_that("$set_native_languages() validates ...", {
    expect_error(trans1$set_native_languages(1L))
    expect_error(trans1$set_native_languages("English"))
    expect_snapshot(trans1$set_native_languages(1L), error = TRUE)
    expect_snapshot(trans1$set_native_languages("English"), error = TRUE)
})

test_that("$rm_block() returns null invisibly", {
    trans <- test_translator()
    expect_null(trans$rm_block("256e0d7"))
    expect_invisible(trans$rm_block("2ac373a"))
})

test_that("$rm_block() throws an error if there are no Block objects to remove", {
    expect_error(Translator$new()$rm_block("error"))
    expect_snapshot(Translator$new()$rm_block("error"), error = TRUE)
})

test_that("$rm_block() validates hash", {
    expect_error(trans1$rm_block(1L))
    expect_error(trans1$rm_block("error"))
    expect_snapshot(trans1$rm_block(1L),      error = TRUE)
    expect_snapshot(trans1$rm_block("error"), error = TRUE)
})

test_that("$rm_block() removes Block objects as expected", {
    trans <- test_translator()
    trans$rm_block("256e0d7")
    expect_length(trans$hashes, 1L)
    expect_identical(trans$hashes, "2ac373aa699a6712cdaddbead28031d537de29bc")
})


# translator() -----------------------------------------------------------------


test_that("translator() returns an R6 object of class Translator", {
    trans <- translator(
        id = "test-translator",
        en = "English",
        fr = "Français",
        block(
            location("a", 1L, 2L, 3L, 4L),
            en = "Hello, world!",
            fr = "Bonjour, monde!"),
        hash_algorithm = "utf8",
        # These arguments should be ignored silently.
        1L, 1.0, 1.0 + 2i, raw(1L))

    expect_s3_class(trans, c("Translator", "R6"), exact = TRUE)
    expect_identical(trans$id, "test-translator")
    expect_identical(trans$hash_algorithm, "utf8")
    expect_identical(trans$hashes, "12351")
    expect_identical(trans$source_texts, c(`12351` = "Hello, world!"))
    expect_identical(trans$languages, c("en", "fr"))
    expect_identical(trans$native_languages, c(en = "English", fr = "Français"))
})

test_that("translator() throws a warning if a language does not have a corresponding native language", {
    expect_warning(
        translator(
            id = "test-translator",
            en = "English",
            block(en = "Hello, world!", fr = "Bonjour, monde!")))
    expect_snapshot(
        translator(
            id = "test-translator",
            en = "English",
            block(en = "Hello, world!", fr = "Bonjour, monde!")))
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
    # values is much more simpler.
    fmt_trans1      <- format(trans1)
    fmt_trans_empty <- format(Translator$new(id = "test-translator"))

    expect_type(fmt_trans1, "character")
    expect_length(fmt_trans1, 9L)
    expect_identical(fmt_trans1, c(
        "<Translator>",
        "  Identifier: test-translator",
        "  Algorithm: sha1",
        "  Languages: ",
        "    en: English",
        "    fr: Français",
        "  Source Texts: ",
        "    256e0d7 [en, fr]: Hello, world!",
        "    2ac373a [en, fr]: Farewell, world!"))

    expect_type(fmt_trans_empty, "character")
    expect_length(fmt_trans_empty, 5L)

    # Check that "<none>" special string is used
    # accordingly when underlying fields are empty.
    # String "<unset>" is theoretically used, but
    # won't ever be shown to the user because of
    # how $initialize() is written.
    expect_identical(fmt_trans_empty, c(
        "<Translator>",
        "  Identifier: test-translator",
        "  Algorithm: sha1",
        "  Languages: <none>",
        "  Source Texts: <none>"))
})


# print.Translator() -----------------------------------------------------------


test_that("print() works", {
    expect_output(print(trans1))
    expect_snapshot(print(trans1))
})

test_that("print() returns x invisibly", {
    withr::local_output_sink(tempfile())
    expect_invisible(print(trans1))
    expect_identical(print(trans1), trans1)
})
