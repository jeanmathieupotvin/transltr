language_source_set("en")
withr::defer(language_source_set(NULL))

test_block <- function() {
    return(
        block(
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
blk2 <- block(
    location("c", 1L, 2L, 3L, 4L),
    location("d", 5L, 6L, 7L, 8L),
    en = "Hello, world!",
    el = "Γεια σου, Κόσμος!")

translate_call <- call("translate")


# Class: active bindings -------------------------------------------------------


test_that("active binding hash returns hash", {
    expect_identical(blk1$hash, "256e0d707386d0fcd9abf10ad994000bdaa25812")
})

test_that("active binding hash throws an error if value is not missing", {
    expect_error(blk1$hash <- "new-hash")
    expect_snapshot(blk1$hash <- "new-hash", error = TRUE)
})

test_that("active binding hash_algorithm returns hashing algorithm", {
    expect_identical(blk1$hash_algorithm, "sha1")
})

test_that("active binding hash_algorithm validates value", {
    expect_error(blk1$hash_algorithm <- 1L)
    expect_error(blk1$hash_algorithm <- "new-algo")
    expect_snapshot(blk1$hash_algorithm <- 1L,         error = TRUE)
    expect_snapshot(blk1$hash_algorithm <- "new-algo", error = TRUE)
})

test_that("active binding hash_algorithm sets new value and new hash", {
    blk <- test_block()
    blk$hash_algorithm <- "utf8"
    expect_identical(blk$hash_algorithm, "utf8")
    expect_identical(blk$hash, "12351")
})

test_that("active binding source_lang returns source language", {
    expect_identical(blk1$source_lang, "en")
})

test_that("active binding source_lang validates value", {
    expect_error(blk1$source_lang <- 1L)
    expect_error(blk1$source_lang <- "new-lang")
    expect_snapshot(blk1$source_lang <- 1L,         error = TRUE)
    expect_snapshot(blk1$source_lang <- "new-lang", error = TRUE)
})

test_that("active binding source_lang sets new value and new hash", {
    blk <- test_block()
    blk$source_lang <- "fr"
    expect_identical(blk$source_lang, "fr")
    expect_identical(blk$hash, "f3c8754329c1b152887d35f00119fca783243d27")
})

test_that("active binding source_text returns source text", {
    expect_identical(blk1$source_text, "Hello, world!")
})

test_that("active binding source_text throws an error if value is not missing", {
    expect_error(blk1$source_text <- "new-text")
    expect_snapshot(blk1$source_text <- "new-text", error = TRUE)
})

test_that("active binding languages returns languages", {
    # This implicitly checks that langs are also sorted.
    langs <- blk1$languages
    expect_identical(langs, c("en", "es", "fr", "ja"), ignore_attr = TRUE)
})

test_that("active binding languages throws an error if value is not missing", {
    expect_error(blk1$languages <- "new-lang")
    expect_snapshot(blk1$languages <- "new-lang", error = TRUE)
})

test_that("active binding translations returns translations", {
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

test_that("active binding locations returns locations", {
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

    expect_s3_class(blk, c("Block", "R6"), exact = TRUE)
    expect_identical(blk$hash_algorithm, "utf8")
    expect_type(blk$.__enclos_env__$private$.translations, "environment")
})

test_that("$initialize() validates hash_algorithm", {
    expect_error(Block$new(1L))
    expect_snapshot(Block$new(1L), error = TRUE)
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
    expect_null(blk$set_translation("en", "Hello, world!"))
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
    expect_null(blk$set_translations())
    expect_invisible(blk$set_translations())

    # Case ... is not empty.
    expect_null(blk$set_translations(en = "Hello, world!"))
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
    expect_null(Block$new()$set_locations())
    expect_invisible(Block$new()$set_locations())

    # Case ... is not empty.
    expect_null(blk$set_locations(location("z", 1L, 1L, 1L, 1L)))
    expect_invisible(blk$set_locations(location("z", 2L, 2L, 2L, 2L)))
    expect_length(blk$locations, 1L)
    expect_identical(
        blk$locations[[1L]],
        location("z", c(1L, 2L), c(1L, 2L), c(1L, 2L), c(1L, 2L)))
})

test_that("$rm_translation() returns a logical", {
    expect_null(test_block()$rm_translation("es"))
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
    expect_null(test_block()$rm_location("a"))
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
    blk <- block(
        location("a"),
        location("b"),
        en = "Hello, world!",
        fr = "Bonjour, monde!",
        # These arguments should be ignored silently.
        1L, 1.0, 1.0 + 2i, raw(1L))

    expect_s3_class(blk, c("Block", "R6"), exact = TRUE)
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
    expect_error(block(source_lang = ""))
    expect_snapshot(block(source_lang = ""), error = TRUE)
})

test_that("block() checks that there is at least one translation corresponding to source_lang", {
    expect_error(block())
    expect_snapshot(block(), error = TRUE)
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

test_that("c.Block() throws an error if source languages are not set", {
    expect_error(c(Block$new(), Block$new()))
    expect_snapshot(c(Block$new(), Block$new()), error = TRUE)
})

test_that("c.Block() does not mutate its arguments", {
    # This test was added after discovering c.Block() was
    # mutating and returning ..1 instead of returning a
    # new object (in a previous implementation).
    blk <- test_block()
    out <- c(blk, blk2)
    expect_identical(blk, test_block())
    expect_identical(blk2, block(
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
    blk1  <- block(location("en"), en = "Hello, world!")
    blk2  <- block(location("el"), en = "Hello, world!", el = "Γεια σου, Κόσμος!")
    blk3  <- block(location("fr"), fr =  "Bonjour, monde!", source_lang = "fr")
    out   <- merge_blocks(blk1, blk2, blk3)
    langs <- vapply_1c(out, `[[`, i = "source_lang")

    expect_length(out, 2L)
    expect_identical(out[[which(langs == "fr")]], blk3)
    expect_identical(out[[which(langs == "en")]], block(
        location("en"),
        location("el"),
        en = "Hello, world!",
        el = "Γεια σου, Κόσμος!"))
})

test_that("merge_blocks() ignores Block objects with no set source language", {
    blk1  <- block(location("en"), en = "Hello, world!")
    blk2  <- block(location("el"), en = "Hello, world!", el = "Γεια σου, Κόσμος!")
    blk3  <- Block$new()
    out   <- merge_blocks(blk1, blk2, blk3)

    expect_length(out, 1L)
    expect_identical(out[[1L]], block(
        location("en"),
        location("el"),
        en = "Hello, world!",
        el = "Γεια σου, Κόσμος!"))
})


# as_block() -------------------------------------------------------------------


test_that("as_block() works", {
    expect_s3_class(as_block(translate_call), "Block")
})


# as_block.call() --------------------------------------------------------------


test_that("as_block.call() returns a Block object", {
    blk <- as_block(
        call("translate", "Hello,", "world!"),
        location       = location("test"),
        hash_algorithm = "utf8")

    expect_s3_class(blk, "Block")
    expect_identical(blk$hash, "12351")
    expect_identical(blk$hash_algorithm, "utf8")
    expect_identical(blk$source_lang, "en")
    expect_identical(blk$source_text, "Hello, world!")
    expect_identical(blk$locations, list(test = location("test")))
})

test_that("as_block.call() validates x", {
    expect_error(as_block(call("block")))
    expect_snapshot(as_block(call("block")), error = TRUE)
})

test_that("as_block.call() validates strict", {
    expect_error(as_block(translate_call, strict = 1L))
    expect_snapshot(as_block(translate_call, strict = 1L), error = TRUE)
})

test_that("as_block.call() validates location", {
    expect_error(as_block(translate_call, location = 1L))
    expect_snapshot(as_block(translate_call, location = 1L), error = TRUE)
})

test_that("as_block.call() validates validate", {
    expect_error(as_block(translate_call, validate = 1L))
    expect_snapshot(as_block(translate_call, validate = 1L), error = TRUE)
})

test_that("as_block.call() extracts ... from x", {
    # The second call is used to test that named
    # arguments passed to ... are tolerated.
    translate_call1 <- call("translate", "Hello, ", "world!")
    translate_call2 <- call("translate", a = "Hello", b = ", world!",
        concat      = "",
        source_lang = "test")

    expect_identical(as_block(translate_call1)$source_text, "Hello, world!")
    expect_identical(as_block(translate_call2)$source_text, "Hello, world!")
})

test_that("as_block.call() extracts concat from x or sets it if not found", {
    translate_call1 <- call("translate", "Hello", ", world!", concat = "")
    translate_call2 <- call("translate", "Hello,", "world!")
    expect_identical(as_block(translate_call1)$source_text, "Hello, world!")
    expect_identical(as_block(translate_call2)$source_text, "Hello, world!")
})

test_that("as_block.call() extracts source_lang from x or sets it if not found", {
    translate_call1 <- call("translate", "Hello", ", world!", source_lang = "test")
    translate_call2 <- call("translate", "Hello,", "world!")
    expect_identical(as_block(translate_call1)$source_lang, "test")
    expect_identical(as_block(translate_call2)$source_lang, "en")
})
