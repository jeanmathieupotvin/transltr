language_source_set("en")
withr::defer(language_source_set(NULL))

test_text <- function() {
    return(
        text(
            location("a", 1L, 2L, 3L, 4L),
            location("a", 1L, 2L, 3L, 4L),
            location("b", 5L, 6L, 7L, 8L),
            en = "Hello, world!",
            fr = "Bonjour, monde!",
            es = "¡Hola Mundo!",
            ja = "こんにちは世界！"))
}

# We use txt1 (and txt2 if two objects are required)
# whenever the underlying test text does not induce
# a permanent state change. Otherwise, a new object
# is generated within the test_that() expression.
txt1 <- test_text()
txt2 <- text(
    location("c", 1L, 2L, 3L, 4L),
    location("d", 5L, 6L, 7L, 8L),
    en = "Hello, world!",
    el = "Γεια σου, Κόσμος!")

translate_call <- call("translate")

# Class: active bindings -------------------------------------------------------

test_that("active binding hash returns hash", {
    expect_identical(txt1$hash, "256e0d707386d0fcd9abf10ad994000bdaa25812")
})

test_that("active binding hash throws an error if value is not missing", {
    expect_error(txt1$hash <- "new-hash")
    expect_snapshot(txt1$hash <- "new-hash", error = TRUE)
})

test_that("active binding algorithm returns hashing algorithm", {
    expect_identical(txt1$algorithm, "sha1")
})

test_that("active binding algorithm validates value", {
    expect_error(txt1$algorithm <- 1L)
    expect_error(txt1$algorithm <- "new-algo")
    expect_snapshot(txt1$algorithm <- 1L,         error = TRUE)
    expect_snapshot(txt1$algorithm <- "new-algo", error = TRUE)
})

test_that("active binding algorithm sets new value and new hash", {
    txt <- test_text()
    txt$algorithm <- "utf8"
    expect_identical(txt$algorithm, "utf8")
    expect_identical(txt$hash, "12351")
})

test_that("active binding source_lang returns source language", {
    expect_identical(txt1$source_lang, "en")
})

test_that("active binding source_lang validates value", {
    expect_error(txt1$source_lang <- 1L)
    expect_error(txt1$source_lang <- "new-lang")
    expect_snapshot(txt1$source_lang <- 1L,         error = TRUE)
    expect_snapshot(txt1$source_lang <- "new-lang", error = TRUE)
})

test_that("active binding source_lang sets new value and new hash", {
    txt <- test_text()
    txt$source_lang <- "fr"
    expect_identical(txt$source_lang, "fr")
    expect_identical(txt$hash, "f3c8754329c1b152887d35f00119fca783243d27")
})

test_that("active binding source_text returns source text", {
    expect_identical(txt1$source_text, "Hello, world!")
})

test_that("active binding source_text throws an error if value is not missing", {
    expect_error(txt1$source_text <- "new-text")
    expect_snapshot(txt1$source_text <- "new-text", error = TRUE)
})

test_that("active binding languages returns languages", {
    # This implicitly checks that langs are also sorted.
    langs <- txt1$languages
    expect_identical(langs, c("en", "es", "fr", "ja"), ignore_attr = TRUE)
})

test_that("active binding languages throws an error if value is not missing", {
    expect_error(txt1$languages <- "new-lang")
    expect_snapshot(txt1$languages <- "new-lang", error = TRUE)
})

test_that("active binding translations returns translations", {
    # This implicitly checks that translations are
    # also sorted by their underlying names (langs).
    expect_identical(txt1$translations, c(
        en = "Hello, world!",
        es = "¡Hola Mundo!",
        fr = "Bonjour, monde!",
        ja = "こんにちは世界！"))
})

test_that("active binding translations throws an error if value is not missing", {
    expect_error(txt1$translations <- "new translation")
    expect_snapshot(txt1$translations <- "new-translation", error = TRUE)
})

test_that("active binding locations returns locations", {
    # This implicitly checks that locations are
    # also sorted by their underlying paths.
    expect_identical(txt1$locations, list(
        a = location("a", 1L, 2L, 3L, 4L),
        b = location("b", 5L, 6L, 7L, 8L)))
})

test_that("active binding locations throws an error if value is not missing", {
    expect_error(txt1$locations <- location())
    expect_snapshot(txt1$locations <- location(), error = TRUE)
})

# Class: public methods --------------------------------------------------------

test_that("$initialize() works", {
    # It can only be tested indirectly via $new().
    txt <- Text$new("utf8")

    expect_s3_class(txt, c("Text", "R6"), exact = TRUE)
    expect_identical(txt$algorithm, "utf8")
    expect_type(txt$.__enclos_env__$private$.translations, "environment")
})

test_that("$get_translation() works", {
    expect_null(txt1$get_translation("error"))
    expect_identical(txt1$get_translation("en"), "Hello, world!")
    expect_identical(txt1$get_translation("es"), "¡Hola Mundo!")
    expect_identical(txt1$get_translation("fr"), "Bonjour, monde!")
    expect_identical(txt1$get_translation("ja"), "こんにちは世界！")
})

test_that("$get_translation() validates lang", {
    expect_error(txt1$get_translation(1L))
    expect_snapshot(txt1$get_translation(1L), error = TRUE)
})

test_that("$set_translation() works", {
    txt <- Text$new()
    expect_null(txt$set_translation("en", "Hello, world!"))
    expect_invisible(txt$set_translation("en", "Hello, world!"))
    expect_identical(txt$get_translation("en"), "Hello, world!")
})

test_that("$set_translation() validates lang", {
    expect_error(txt1$set_translation(1L))
    expect_snapshot(txt1$set_translation(1L), error = TRUE)
})

test_that("$set_translation() validates text", {
    expect_error(txt1$set_translation("de", 1L))
    expect_snapshot(txt1$set_translation("de", 1L), error = TRUE)
})

test_that("$set_translations() works", {
    txt <- Text$new()

    # Case ... is empty.
    expect_null(txt$set_translations())
    expect_invisible(txt$set_translations())

    # Case ... is not empty.
    expect_null(txt$set_translations(en = "Hello, world!"))
    expect_invisible(txt$set_translations(fr = "Bonjour, monde!"))
    expect_identical(txt$get_translation("en"), "Hello, world!")
    expect_identical(txt$get_translation("fr"), "Bonjour, monde!")
})

test_that("$set_translations() validates ...", {
    txt <- Text$new()
    expect_error(txt$set_translations(1L))
    expect_error(txt$set_translations("Hello, world!"))
    expect_snapshot(txt$set_translations(1L),              error = TRUE)
    expect_snapshot(txt$set_translations("Hello, world!"), error = TRUE)
})

test_that("$set_locations() returns a logical", {
    txt <- Text$new()

    # Case ... is empty.
    expect_null(Text$new()$set_locations())
    expect_invisible(Text$new()$set_locations())

    # Case ... is not empty.
    expect_null(txt$set_locations(location("z", 1L, 1L, 1L, 1L)))
    expect_invisible(txt$set_locations(location("z", 2L, 2L, 2L, 2L)))
    expect_length(txt$locations, 1L)
    expect_identical(
        txt$locations[[1L]],
        location("z", c(1L, 2L), c(1L, 2L), c(1L, 2L), c(1L, 2L)))
})

test_that("$rm_translation() returns a logical", {
    expect_null(test_text()$rm_translation("es"))
    expect_invisible(test_text()$rm_translation("fr"))
})

test_that("$rm_translation() validates lang", {
    expect_error(txt1$rm_translation(1L))
    expect_error(txt1$rm_translation("en"))
    expect_error(txt1$rm_translation("error"))
    expect_snapshot(txt1$rm_translation(1L),      error = TRUE)
    expect_snapshot(txt1$rm_translation("en"),    error = TRUE)
    expect_snapshot(txt1$rm_translation("error"), error = TRUE)
})

test_that("$rm_translation() removes translations as expected", {
    txt <- test_text()
    txt$rm_translation("es")
    expect_length(txt$translations, 3L)
    expect_identical(txt$translations, c(
        en = "Hello, world!",
        fr = "Bonjour, monde!",
        ja = "こんにちは世界！"))
})

test_that("$rm_location() returns a logical", {
    expect_null(test_text()$rm_location("a"))
    expect_invisible(test_text()$rm_location("a"))
})

test_that("$rm_location() validates path", {
    expect_error(txt1$rm_location(1L))
    expect_error(txt1$rm_location("error"))
    expect_snapshot(txt1$rm_location(1L),      error = TRUE)
    expect_snapshot(txt1$rm_location("error"), error = TRUE)
})

test_that("$rm_location() removes locations as expected", {
    txt <- test_text()
    txt$rm_location("a")
    expect_length(txt$locations, 1L)
    expect_identical(txt$locations, list(b = location("b", 5L, 6L, 7L, 8L)))
})

# text() -----------------------------------------------------------------------

test_that("text() returns an R6 object of class Text", {
    txt <- text(
        location("a"),
        location("b"),
        en = "Hello, world!",
        fr = "Bonjour, monde!",
        # These arguments should be ignored silently.
        1L, 1.0, 1.0 + 2i, raw(1L))

    expect_s3_class(txt, c("Text", "R6"), exact = TRUE)
    expect_identical(txt$hash, "256e0d707386d0fcd9abf10ad994000bdaa25812")
    expect_identical(txt$algorithm, "sha1")
    expect_identical(txt$source_lang, "en")
    expect_identical(txt$source_text, "Hello, world!")
    expect_identical(txt$translations, c(
        en = "Hello, world!",
        fr = "Bonjour, monde!"))
    expect_identical(txt$locations, list(a = location("a"), b = location("b")))
})

test_that("text() validates source_lang", {
    expect_error(text(source_lang = ""))
    expect_snapshot(text(source_lang = ""), error = TRUE)
})

test_that("text() checks that there is at least one translation corresponding to source_lang", {
    expect_error(text())
    expect_snapshot(text(), error = TRUE)
})

# is_text() --------------------------------------------------------------------

test_that("is_text() works", {
    expect_true(is_text(Text$new()))
    expect_false(is_text(1L))
})

# format.Text() ----------------------------------------------------------------

test_that("format() returns a character", {
    # This test block is a little bit
    # fragile, but hardcoding expected
    # values is simpler.
    fmt_txt2      <- format(txt2)
    fmt_txt_empty <- format(Text$new())

    expect_type(fmt_txt2, "character")
    expect_length(fmt_txt2, 16L)
    expect_identical(fmt_txt2, c(
        "<Text>",
        " Hash: 256e0d707386d0fcd9abf10ad994000bdaa25812",
        " Source Lang: en",
        " Algorithm: sha1",
        " Translations:",
        "  el: Γεια σου, Κόσμος!",
        "  en: Hello, world!",
        " Locations:",
        "  c:",
        "   <Location>",
        "    Path: c",
        "    Ranges: Ln 1, Col 2 @ Ln 3, Col 4",
        "  d:",
        "   <Location>",
        "    Path: d",
        "    Ranges: Ln 5, Col 6 @ Ln 7, Col 8"))

    expect_type(fmt_txt_empty, "character")
    expect_length(fmt_txt_empty, 6L)
    expect_identical(fmt_txt_empty, c(
        "<Text>",
        " Hash: <unset>",
        " Source Lang: <unset>",
        " Algorithm: sha1",
        " Translations: <null>",
        " Locations: <empty> [list]"))
})

test_that("format() sets names of locations equal to base names", {
    txt <- text(
        location("/absolute/path/to/source/script/c", 1L, 2L, 3L, 4L),
        location("/absolute/path/to/source/script/d", 5L, 6L, 7L, 8L),
        en = "Hello, world!",
        el = "Γεια σου, Κόσμος!")

    expect_identical(format(txt), c(
        "<Text>",
        " Hash: 256e0d707386d0fcd9abf10ad994000bdaa25812",
        " Source Lang: en",
        " Algorithm: sha1",
        " Translations:",
        "  el: Γεια σου, Κόσμος!",
        "  en: Hello, world!",
        " Locations:",
        "  c:",
        "   <Location>",
        "    Path: /absolute/path/to/source/script/c",
        "    Ranges: Ln 1, Col 2 @ Ln 3, Col 4",
        "  d:",
        "   <Location>",
        "    Path: /absolute/path/to/source/script/d",
        "    Ranges: Ln 5, Col 6 @ Ln 7, Col 8"))
    expect_snapshot(print(txt))
})

test_that("format() escapes newlines", {
    txt <- text(
        en = "Hello,\n\nworld!",
        el = "Γεια σου,\n\nΚόσμος!")
    out <- format(txt)

    expect_match(out[[6L]], r"{\\n\\n}")  ## el
    expect_match(out[[7L]], r"{\\n\\n}")  ## en
    expect_snapshot(print(txt))
})

# print.Text() -----------------------------------------------------------------

test_that("print() works", {
    expect_output(print(txt1))
    expect_snapshot(print(txt1))
})

test_that("print() returns x invisibly", {
    withr::local_output_sink(tempfile())
    expect_invisible(print(txt1))
    expect_identical(print(txt1), txt1)
})

# c.Text() ---------------------------------------------------------------------

test_that("c.Text() returns a Text object", {
    out <- c(txt1, txt2)

    expect_s3_class(out, "Text")
    expect_identical(out$hash, txt1$hash)
    expect_identical(out$algorithm, txt1$algorithm)
    expect_identical(out$source_lang, txt1$source_lang)
    expect_identical(out$source_text, txt1$source_text)
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

test_that("c.Text() returns its single argument", {
    expect_identical(c(txt1), txt1)
})

test_that("c.Text() validates ...", {
    # The first argument passed to c() must be
    # a Text object. Otherwise, S3 dispatching
    # won't work as expected.
    expect_error(c(txt1, 1L, txt2))
    expect_snapshot(c(txt1, 1L, txt2), error = TRUE)
})

test_that("c.Text() throws an error if hashes are not equal", {
    txt <- test_text()
    txt$source_lang <- "fr"

    expect_error(c(txt1, txt))
    expect_snapshot(error = TRUE, {
        txt1 <- test_text()
        txt2 <- test_text()
        txt2$source_lang <- "fr"
        c(txt1, txt2)
    })
})

test_that("c.Text() throws an error if source languages are not set", {
    expect_error(c(Text$new(), Text$new()))
    expect_snapshot(c(Text$new(), Text$new()), error = TRUE)
})

test_that("c.Text() does not mutate its arguments", {
    # This test was added after discovering c.Text() was
    # mutating and returning ..1 instead of returning a
    # new object (in a previous implementation).
    txt <- test_text()
    out <- c(txt, txt2)
    expect_identical(txt, test_text())
    expect_identical(txt2, text(
        location("c", 1L, 2L, 3L, 4L),
        location("d", 5L, 6L, 7L, 8L),
        en = "Hello, world!",
        el = "Γεια σου, Κόσμος!"))
})

# merge_texts() ----------------------------------------------------------------

test_that("merge_texts() returns a list of Text object", {
    txt <- test_text()
    txt$source_lang <- "ja"
    out <- merge_texts(txt1, txt)

    expect_type(out, "list")
    expect_length(out, 2L)
})

test_that("merge_texts() validates ...", {
    expect_error(merge_texts(txt1, 1L, txt2))
    expect_snapshot(merge_texts(txt1, 1L, txt2), error = TRUE)
})

test_that("merge_texts() validates algorithm", {
    expect_error(merge_texts(txt1, txt2, algorithm = "error"))
    expect_snapshot(merge_texts(txt1, txt2, algorithm = "error"), error = TRUE)
})

test_that("merge_texts() combines Text objects having different hashes", {
    txt1  <- text(location("en"), en = "Hello, world!")
    txt2  <- text(location("el"), en = "Hello, world!", el = "Γεια σου, Κόσμος!")
    txt3  <- text(location("fr"), fr =  "Bonjour, monde!", source_lang = "fr")
    out   <- merge_texts(txt1, txt2, txt3)
    langs <- vapply_1c(out, `[[`, i = "source_lang")

    expect_length(out, 2L)
    expect_identical(out[[which(langs == "fr")]], txt3)
    expect_identical(out[[which(langs == "en")]], text(
        location("en"),
        location("el"),
        en = "Hello, world!",
        el = "Γεια σου, Κόσμος!"))
})

test_that("merge_texts() ignores Text objects with no set source language", {
    txt1  <- text(location("en"), en = "Hello, world!")
    txt2  <- text(location("el"), en = "Hello, world!", el = "Γεια σου, Κόσμος!")
    txt3  <- Text$new()
    out   <- merge_texts(txt1, txt2, txt3)

    expect_length(out, 1L)
    expect_identical(out[[1L]], text(
        location("en"),
        location("el"),
        en = "Hello, world!",
        el = "Γεια σου, Κόσμος!"))
})

# as_text() --------------------------------------------------------------------

test_that("as_text() works", {
    expect_s3_class(as_text(translate_call), "Text")
})

# as_text.call() ---------------------------------------------------------------

test_that("as_text.call() returns a Text object", {
    txt <- as_text(
        call("translate", "Hello, world!"),
        loc       = location("test"),
        algorithm = "utf8")

    expect_s3_class(txt, "Text")
    expect_identical(txt$hash, "12351")
    expect_identical(txt$algorithm, "utf8")
    expect_identical(txt$source_lang, "en")
    expect_identical(txt$source_text, "Hello, world!")
    expect_identical(txt$locations, list(test = location("test")))
})

test_that("as_text.call() validates loc", {
    expect_error(as_text(translate_call, loc = 1L))
    expect_snapshot(as_text(translate_call, loc = 1L), error = TRUE)
})

test_that("as_text.call() extracts ... from x", {
    translate_call1 <- call("translate", "Hello,", "world!")

    expect_identical(as_text(translate_call1)$source_text, "Hello,\n\nworld!")
})

test_that("as_text.call() extracts source_lang from x or sets it if not found", {
    translate_call1 <- call("translate", "Hello", ", world!", source_lang = "test")
    translate_call2 <- call("translate", "Hello,", "world!")
    expect_identical(as_text(translate_call1)$source_lang, "test")
    expect_identical(as_text(translate_call2)$source_lang, "en")
})

test_that("as_text.call() sets source fields only if ... is not empty", {
    txt <- as_text(translate_call)
    expect_null(txt$source_text)
    expect_identical(txt$source_lang, .__STR_UNSET)
})
