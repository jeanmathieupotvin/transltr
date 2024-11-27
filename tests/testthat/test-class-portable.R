language_source_set("en")
withr::defer(language_source_set(NULL))

txt <- text(
    en = "Hello, world!",
    fr = "Bonjour, monde!",
    location("test-file-1", 1L, 2L, 3L, 4L),
    location("test-file-2", 1L, 2L, 3L, 4L),
    hash_algorithm = "utf8")


# portable() -------------------------------------------------------------------


test_that("portable() returns x", {
    expect_identical(portable(1L), 1L, ignore_attr = TRUE)
})

test_that("portable() returns a Portable object", {
    expect_s3_class(portable(1L), "Portable")
})

test_that("portable() validates super", {
    expect_error(portable(1L, super = 1L))
    expect_snapshot(portable(1L, super = 1L), error = TRUE)
})

test_that("portable() validates tag if not null", {
    expect_error(portable(1L, tag = 1L))
    expect_snapshot(portable(1L, tag = 1L), error = TRUE)
})

test_that("portable() adds a tag attribute if not null", {
    expect_identical(attr(portable(1L, tag = "Integer"), "tag"), "Integer")
})

test_that("portable() adds super-classes if any", {
    super_classes <- c("SuperPortableInteger", "PortableInteger")
    expect_s3_class(portable(1L, super_classes), super_classes)
})


# is_portable() ----------------------------------------------------------------


test_that("is_portable() returns a logical", {
    expect_true(is_portable(portable(1L)))
    expect_false(is_portable(1L))
})


# portable_text() --------------------------------------------------------------


test_that("portable_text() returns an S3 object of class PortableText", {
    out <- portable_text(txt)

    expect_s3_class(out, "PortableText")
    expect_identical(attr(out, "tag"), "Text")
    expect_identical(out$hash,            txt$hash)
    expect_identical(out$hash_algorithm,  txt$hash_algorithm)
    expect_identical(out$source_language, txt$source_lang)
    expect_identical(out$source_text,     txt$source_text)

    # $locations should not have names.
    expect_identical(out$locations, list(
        portable_location(location("test-file-1", 1L, 2L, 3L, 4L)),
        portable_location(location("test-file-2", 1L, 2L, 3L, 4L))))

    # $translations is defined but NULL by default.
    expect_in("translations", names(out))
    expect_null(out$translations)
})

test_that("portable_text() validates x", {
    expect_error(portable_text(1L))
    expect_snapshot(portable_text(1L), error = TRUE)
})

test_that("portable_text() validates set_translations", {
    expect_error(portable_text(Text$new(), 1L))
    expect_snapshot(portable_text(Text$new(), 1L), error = TRUE)
})

test_that("portable_text() sets translations if set_translations is true", {
    out <- portable_text(txt, TRUE)

    expect_identical(out$translations, list(fr = "Bonjour, monde!"))
})

test_that("portable_text() wraps source text and translations", {
    # The following strings are longer than 80 characters on purpose.
    txt <- text(
        en = "Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.",
        fr = "Le Lorem Ipsum est le faux texte standard de l'imprimerie depuis les années 1500, quand un imprimeur anonyme assembla ensemble des morceaux de texte pour réaliser un livre spécimen de polices de texte.")
    out <- portable_text(txt, TRUE)

    expect_length(out$source_language, 1L)
    expect_length(out$translations$fr, 1L)
    expect_snapshot({
        out$source_text
        out$translations$fr
    })
})

test_that("portable_text() updates hash, source_language, and source_text only if hash is set", {
    out <- portable_text(Text$new())

    expect_null(out$hash)
    expect_null(out$source_language)
    expect_null(out$source_text)
})


# portable_location() ----------------------------------------------------------


test_that("portable_location() returns an S3 object of class PortableLocation", {
    loc <- location("test-file", 1L, 2L, 3L, 4L)
    out <- portable_location(loc)

    expect_s3_class(out, "PortableLocation")
    expect_identical(attr(out, "tag"), "Location")
    expect_identical(out$path, out$path)
    expect_identical(out$ranges, "line 1, column 2 @ line 3, column 4")
})

test_that("portable_location() validates x", {
    expect_error(portable_location(1L))
    expect_snapshot(portable_location(1L), error = TRUE)
})


# Degenerate as_*() methods ----------------------------------------------------


test_that("as_location.Location() works", {
    loc <- location()
    expect_identical(as_location(loc), loc)
})

test_that("as_text.Text() works", {
    txt <- text(en = "Hello, world!")
    expect_identical(as_text(txt), txt)
})
