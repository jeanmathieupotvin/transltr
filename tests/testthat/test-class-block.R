test_block <- function() {
    return(
        block("en",
            location("a", 1L, 2L, 3L, 4L),
            location("a", 1L, 2L, 3L, 4L),
            location("b", 5L, 6L, 7L, 8L),
            en = "Hello, world!",
            fr = "Bonjour, monde!",
            es = "¡Hola Mundo!",
            ja = "こんにちは世界！"))
}

# We use blk1 (and blk2 if two objects are required)
# whenever the underlying test block does not induce
# a permanent state change. Otherwise, a new object
# is generated within the test_that() expression.
blk1 <- test_block()
blk2 <- block("en",
    location("c", 1L, 2L, 3L, 4L),
    location("d", 5L, 6L, 7L, 8L),
    en = "Hello, world!",
    el = "Γεια σου, Κόσμος!")


# Class: active bindings -------------------------------------------------------


test_that("active binding hash returns registered hash", {
    expect_identical(blk1$hash, "256e0d707386d0fcd9abf10ad994000bdaa25812")
})

test_that("active binding hash throws an error if value is not missing", {
    expect_error(blk1$hash <- "new-hash")
    expect_snapshot(blk1$hash <- "new-hash", error = TRUE)
})

test_that("active binding hash_algorithm returns registered hash_algorithm", {
    expect_identical(blk1$hash_algorithm, "sha1")
})

test_that("active binding hash_algorithm validates value", {
    expect_error(blk1$hash_algorithm <- 1L)
    expect_error(blk1$hash_algorithm <- "new-algo")
    expect_snapshot(blk1$hash_algorithm <- "new-algo", error = TRUE)
})

test_that("active binding hash_algorithm sets new value and new hash", {
    blk <- test_block()
    blk$hash_algorithm <- "utf8"
    expect_identical(blk$hash_algorithm, "utf8")
    expect_identical(blk$hash, "12351")
})

test_that("active binding source_lang returns registered source_lang", {
    expect_identical(blk1$source_lang, "en")
})

test_that("active binding source_lang validates value", {
    expect_error(blk1$source_lang <- 1L)
    expect_error(blk1$source_lang <- "new-lang")
    expect_snapshot(blk1$source_lang <- "new-lang", error = TRUE)
})

test_that("active binding source_lang sets new value and new hash", {
    blk <- test_block()
    blk$source_lang <- "fr"
    expect_identical(blk$source_lang, "fr")
    expect_identical(blk$hash, "f3c8754329c1b152887d35f00119fca783243d27")
})

test_that("active binding source_text returns registered source_text", {
    expect_identical(blk1$source_text, "Hello, world!")
})

test_that("active binding source_text throws an error if value is not missing", {
    expect_error(blk1$source_text <- "new-text")
    expect_snapshot(blk1$source_text <- "new-text", error = TRUE)
})

test_that("active binding languages returns registered languages", {
    # This implicitly checks that langs are also sorted.
    langs <- blk1$languages
    expect_identical(langs, c("en", "es", "fr", "ja"), ignore_attr = TRUE)
    expect_identical(attr(langs, "source_lang"), "en")
})

test_that("active binding languages throws an error if value is not missing", {
    expect_error(blk1$langs <- "new-lang")
    expect_snapshot(blk1$langs <- "new-lang", error = TRUE)
})

test_that("active binding translations returns registered translations", {
    # This implicitly checks that translations are
    # also sorted by their underlying names (langs).
    expect_identical(blk1$translations, c(
        en = "Hello, world!",
        es = "¡Hola Mundo!",
        fr = "Bonjour, monde!",
        ja = "こんにちは世界！"))
})

test_that("active binding translations throws an error if value is not missing", {
    expect_error(blk1$translations <- "new translation")
    expect_snapshot(blk1$translations <- "new-translation", error = TRUE)
})

test_that("active binding locations returns registered locations", {
    # This implicitly checks that locations are
    # also sorted by their underlying paths.
    expect_identical(blk1$locations, list(
        a = location("a", 1L, 2L, 3L, 4L),
        b = location("b", 5L, 6L, 7L, 8L)))
})

test_that("active binding locations throws an error if value is not missing", {
    expect_error(blk1$locations <- location())
    expect_snapshot(blk1$locations <- location(), error = TRUE)
})


# Class: public methods --------------------------------------------------------


test_that("$initialize() works", {
    # It can only be tested indirectly via $new().
    blk <- Block$new("utf8")
    expect_identical(blk$hash_algorithm, "utf8")
    expect_type(blk$.__enclos_env__$private$.translations, "environment")
})

test_that("$initialize() validates hash_algorithm", {
    expect_error(Block$new("error"))
    expect_snapshot(Block$new("error"), error = TRUE)
})

test_that("$get_translation() works", {
    expect_null(blk1$get_translation("error"))
    expect_identical(blk1$get_translation("en"), "Hello, world!")
    expect_identical(blk1$get_translation("es"), "¡Hola Mundo!")
    expect_identical(blk1$get_translation("fr"), "Bonjour, monde!")
    expect_identical(blk1$get_translation("ja"), "こんにちは世界！")
})

test_that("$get_translation() validates lang", {
    expect_error(blk1$get_translation(1L))
    expect_snapshot(blk1$get_translation(1L), error = TRUE)
})

test_that("$set_translation() works", {
    blk <- Block$new()
    expect_true(blk$set_translation("en", "Hello, world!"))
    expect_invisible(blk$set_translation("en", "Hello, world!"))
    expect_identical(blk$get_translation("en"), "Hello, world!")
})

test_that("$set_translation() validates lang", {
    expect_error(blk1$set_translation(1L))
    expect_snapshot(blk1$set_translation(1L), error = TRUE)
})

test_that("$set_translation() validates text", {
    expect_error(blk1$set_translation("de", 1L))
    expect_snapshot(blk1$set_translation("de", 1L), error = TRUE)
})

test_that("$set_translations() works", {
    blk <- Block$new()

    # Case ... is empty.
    expect_true(Block$new()$set_translations())
    expect_invisible(Block$new()$set_translations())

    # Case ... is not empty.
    expect_true(blk$set_translations(en = "Hello, world!"))
    expect_invisible(blk$set_translations(fr = "Bonjour, monde!"))
    expect_identical(blk$get_translation("en"), "Hello, world!")
    expect_identical(blk$get_translation("fr"), "Bonjour, monde!")
})

test_that("$set_translations() validates ...", {
    blk <- Block$new()
    expect_error(blk$set_translations(1L))
    expect_error(blk$set_translations("Hello, world!"))
    expect_snapshot(blk$set_translations(1L),              error = TRUE)
    expect_snapshot(blk$set_translations("Hello, world!"), error = TRUE)
})

test_that("$set_locations() returns a logical", {
    blk <- Block$new()

    # Case ... is empty.
    expect_true(Block$new()$set_locations())
    expect_invisible(Block$new()$set_locations())

    # Case ... is not empty.
    expect_true(blk$set_locations(location("z", 1L, 1L, 1L, 1L)))
    expect_invisible(blk$set_locations(location("z", 2L, 2L, 2L, 2L)))
    expect_length(blk$locations, 1L)
    expect_identical(
        blk$locations[[1L]],
        location("z", c(1L, 2L), c(1L, 2L), c(1L, 2L), c(1L, 2L)))
})

test_that("$rm_translation() returns a logical", {
    expect_true(test_block()$rm_translation("es"))
    expect_invisible(test_block()$rm_translation("fr"))
})

test_that("$rm_translation() validates lang", {
    expect_error(blk1$rm_translation(1L))
    expect_error(blk1$rm_translation("en"))
    expect_error(blk1$rm_translation("error"))
    expect_snapshot(blk1$rm_translation(1L),      error = TRUE)
    expect_snapshot(blk1$rm_translation("en"),    error = TRUE)
    expect_snapshot(blk1$rm_translation("error"), error = TRUE)
})

test_that("$rm_translation() removes translations as expected", {
    blk <- test_block()
    blk$rm_translation("es")
    expect_length(blk$translations, 3L)
    expect_identical(blk$translations, c(
        en = "Hello, world!",
        fr = "Bonjour, monde!",
        ja = "こんにちは世界！"))
})

test_that("$rm_location() returns a logical", {
    expect_true(test_block()$rm_location("a"))
    expect_invisible(test_block()$rm_location("a"))
})

test_that("$rm_location() validates path", {
    expect_error(blk1$rm_location(1L))
    expect_error(blk1$rm_location("error"))
    expect_snapshot(blk1$rm_location(1L),      error = TRUE)
    expect_snapshot(blk1$rm_location("error"), error = TRUE)
})

test_that("$rm_location() removes locations as expected", {
    blk <- test_block()
    blk$rm_location("a")
    expect_length(blk$locations, 1L)
    expect_identical(blk$locations, list(b = location("b", 5L, 6L, 7L, 8L)))
})


# block() ----------------------------------------------------------------------


test_that("block() returns an R6 object of class Block", {
    blk <- block("en",
        location("a"),
        location("b"),
        en = "Hello, world!",
        fr = "Bonjour, monde!",
        # These arguments should be ignored silently.
        1L, 1.0, 1.0 + 2i, raw(1L))

    expect_s3_class(blk, "Block")
    expect_identical(blk$hash, "256e0d707386d0fcd9abf10ad994000bdaa25812")
    expect_identical(blk$hash_algorithm, "sha1")
    expect_identical(blk$source_lang, "en")
    expect_identical(blk$source_text, "Hello, world!")
    expect_identical(blk$translations, c(
        en = "Hello, world!",
        fr = "Bonjour, monde!"))
    expect_identical(blk$locations, list(a = location("a"), b = location("b")))
})

test_that("block() validates source_lang", {
    expect_error(block(""))
    expect_snapshot(block(""), error = TRUE)
})

test_that("block() checks that there is at least one translation corresponding to source_lang", {
    expect_error(block("en"))
    expect_snapshot(block("en"), error = TRUE)
})


# .block() ---------------------------------------------------------------------


test_that(".block() returns an R6 object of class Block", {
    blk <- .block(
        source_lang    = "en",
        source_text    = "Hello, world!",
        hash_algorithm = "sha1",
        langs          = "fr",
        texts          = "Bonjour, monde!",
        locations      = list(location("a"), location("b")))

    expect_s3_class(blk, "Block")
    expect_identical(blk$hash, "256e0d707386d0fcd9abf10ad994000bdaa25812")
    expect_identical(blk$hash_algorithm, "sha1")
    expect_identical(blk$source_lang, "en")
    expect_identical(blk$source_text, "Hello, world!")
    expect_identical(blk$translations, c(
        en = "Hello, world!",
        fr = "Bonjour, monde!"))
    expect_identical(blk$locations, list(a = location("a"), b = location("b")))
})

test_that(".block() validates source_lang", {
    expect_error(.block(""))
    expect_snapshot(.block(""), error = TRUE)
})

test_that(".block() validates source_text", {
    expect_error(.block("en", 1L))
    expect_snapshot(.block("en", 1L), error = TRUE)
})

test_that(".block() validates langs", {
    expect_error(.block("en", "Hello, world!", langs = 1L))
    expect_snapshot(.block("en", "Hello, world!", langs = 1L), error = TRUE)
})

test_that(".block() validates texts", {
    expect_error(.block("en", "Hello, world!", texts = 1L))
    expect_snapshot(.block("en", "Hello, world!", texts = 1L), error = TRUE)
})

test_that(".block() validates lengths of langs and trans_texts", {
    expect_error(.block("en", trans_langs = "en"))
    expect_error(.block("en", trans_texts = "Hello, world!"))
    expect_snapshot(.block("en", trans_langs = "en"), error = TRUE)
})

test_that(".block() handles non-list values passed to locations", {
    # Any value that is not a Location (within a list or not)
    # should result in an error thrown by $set_locations().
    expect_no_condition(.block("en", locations = list(location())))
    expect_no_condition(.block("en", locations = location()))
    expect_no_condition(.block("en", locations = list()))
    expect_error(.block("en", locations = 1L))
    expect_error(.block("en", locations = list(1L)))
    expect_error(.block("en", locations = list(1L, location())))
})


# is_block() -------------------------------------------------------------------


test_that("is_block() works", {
    expect_true(is_block(Block$new()))
    expect_false(is_block(1L))
})


# format.Block() ---------------------------------------------------------------


test_that("format() returns a character", {
    # This test block is a little bit
    # fragile, but hardcoding expected
    # values is much more simpler.
    fmt_blk2      <- format(blk2)
    fmt_blk_empty <- format(Block$new())

    expect_type(fmt_blk2, "character")
    expect_length(fmt_blk2, 14L)
    expect_identical(fmt_blk2, c(
        "<Block>",
        "  Hash: 256e0d707386d0fcd9abf10ad994000bdaa25812",
        "  Source Lang: en",
        "  Algorithm: sha1",
        "  Translations: ",
        "    el: Γεια σου, Κόσμος!",
        "    en: Hello, world!",
        "  Locations: ",
        "    <Location>",
        "      Path: c",
        "      Ranges: line 1, column 2 @ line 3, column 4",
        "    <Location>",
        "      Path: d",
        "      Ranges: line 5, column 6 @ line 7, column 8"))

    expect_type(fmt_blk_empty, "character")
    expect_length(fmt_blk_empty, 6L)

    # Check that "<unset>" and "<none>"
    # special strings are used accordingly
    # when underlying fields are empty.
    expect_identical(fmt_blk_empty, c(
        "<Block>",
        "  Hash: <unset>",
        "  Source Lang: <unset>",
        "  Algorithm: sha1",
        "  Translations: <none>",
        "  Locations: <none>"))
})


# print.Block() ----------------------------------------------------------------


test_that("print() works", {
    expect_output(print(blk1))
    expect_snapshot(print(blk1))
})

test_that("print() returns x invisibly", {
    withr::local_output_sink(tempfile())
    expect_invisible(print(blk1))
    expect_identical(print(blk1), blk1)
})


# c.Block() --------------------------------------------------------------------


test_that("c.Block() returns a Block object", {
    out <- c(blk1, blk2)

    expect_s3_class(out, "Block")
    expect_identical(out$hash, blk1$hash)
    expect_identical(out$hash_algorithm, blk1$hash_algorithm)
    expect_identical(out$source_lang, blk1$source_lang)
    expect_identical(out$source_text, blk1$source_text)
    expect_identical(out$translations, c(
        el = "Γεια σου, Κόσμος!",
        en = "Hello, world!",
        es = "¡Hola Mundo!",
        fr = "Bonjour, monde!",
        ja = "こんにちは世界！"))
    expect_identical(out$locations, list(
        a = location("a", 1L, 2L, 3L, 4L),
        b = location("b", 5L, 6L, 7L, 8L),
        c = location("c", 1L, 2L, 3L, 4L),
        d = location("d", 5L, 6L, 7L, 8L)))
})

test_that("c.Block() returns its single argument", {
    expect_identical(c(blk1), blk1)
})

test_that("c.Block() validates ...", {
    # The first argument passed to c() must be
    # a Block object. Otherwise, S3 dispatching
    # won't work as expected.
    expect_error(c(blk1, 1L, blk2))
    expect_snapshot(c(blk1, 1L, blk2), error = TRUE)
})

test_that("c.Block() throws an error if hashes are not equal", {
    blk <- test_block()
    blk$source_lang <- "fr"

    expect_error(c(blk1, blk))
    expect_snapshot(error = TRUE, {
        blk1 <- test_block()
        blk2 <- test_block()
        blk2$source_lang <- "fr"
        c(blk1, blk2)
    })
})

test_that("c.Block() does not mutate its arguments", {
    # This test was added after discovering c.Block() was
    # mutating and returning ..1 instead of returning a
    # new object (in a previous implementation).
    blk <- test_block()
    out <- c(blk, blk2)
    expect_identical(blk, test_block())
    expect_identical(blk2, block(
        "en",
        location("c", 1L, 2L, 3L, 4L),
        location("d", 5L, 6L, 7L, 8L),
        en = "Hello, world!",
        el = "Γεια σου, Κόσμος!"))
})


# merge_blocks() ---------------------------------------------------------------


test_that("merge_blocks() returns a list of Block object", {
    blk <- test_block()
    blk$source_lang <- "ja"
    out <- merge_blocks(blk1, blk)

    expect_type(out, "list")
    expect_length(out, 2L)
})

test_that("merge_blocks() validates ...", {
    expect_error(merge_blocks(blk1, 1L, blk2))
    expect_snapshot(merge_blocks(blk1, 1L, blk2), error = TRUE)
})

test_that("merge_blocks() validates hash_algorithm", {
    expect_error(merge_blocks(blk1, blk2, hash_algorithm = "error"))
    expect_snapshot(merge_blocks(blk1, blk2, hash_algorithm = "error"), error = TRUE)
})

test_that("merge_blocks() combines Block objects having different hashes", {
    blk1  <- block("en", location("en"), en = "Hello, world!")
    blk2  <- block("en", location("el"), en = "Hello, world!", el = "Γεια σου, Κόσμος!")
    blk3  <- block("fr", location("fr"), fr =  "Bonjour, monde!")
    out   <- merge_blocks(blk1, blk2, blk3)
    langs <- vapply_1c(out, `[[`, i = "source_lang")

    expect_length(out, 2L)
    expect_identical(out[[which(langs == "fr")]], blk3)
    expect_identical(out[[which(langs == "en")]], block(
        "en",
        location("en"),
        location("el"),
        en = "Hello, world!",
        el = "Γεια σου, Κόσμος!"))
})


# as_block() -------------------------------------------------------------------


# FIXME: To be tested later once the API matures. This function will likely
# change in a near future.
