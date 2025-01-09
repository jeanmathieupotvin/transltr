language_source_set("en")
withr::defer(language_source_set(NULL))

tr <- translator(
    id = "test-translator",
    el = "Ελληνικά",
    en = "English",
    es = "Español",
    fr = "Français",
    text(
        location("a", 1L, 2L, 3L, 4L),
        en = "Hello, world!",
        es = "¡Hola Mundo!",
        fr = "Bonjour, monde!"),
    text(
        location("b", 5L, 6L, 7L, 8L),
        en = "Farewell, world!",
        fr = "Au revoir, monde!"))

txt <- text(
    location("a", 1L, 2L, 3L, 4L),
    location("b", 5L, 6L, 7L, 8L),
    en = "Hello, world!",
    fr = "Bonjour, monde!",
    es = "¡Hola Mundo!",
    ja = "こんにちは世界！")

loc <- location(
    path  = "tests/testthat/my-test-file",
    line1 = c(1L,  11L),
    col1  = c(22L, 222L),
    line2 = c(10L, 3333L),
    col2  = c(1L,  4L))


# export() ---------------------------------------------------------------------


test_that("export() works", {
    expect_s3_class(export(loc), "ExportedLocation")
    expect_s3_class(export(txt), "ExportedText")
    expect_s3_class(export(tr),  "ExportedTranslator")
})


# export.Text() ----------------------------------------------------------------


test_that("export.Text() returns a named list of S3 class ExportedText", {
    out <- export(txt)

    expect_s3_class(out, "ExportedText")
    expect_identical(attr(out, "tag"), "ExportedText")
    expect_type(out, "list")
    expect_length(out, 7L)

    # _Uuid is random and cannot be compared to a reference value.
    expect_type(out$`_Uuid`, "character")
    expect_length(out$`_Uuid`, 1L)

    expect_identical(out$`Hashing Algorithm`, txt$hash_algorithm)
    expect_identical(out$Hash, txt$hash)
    expect_identical(out$`Source Language`, txt$source_lang)
    expect_identical(out$`Source Text`, txt$source_text)

    # Translations are not included by default.
    expect_null(out$Translations)

    # All Location objects should be exported and unnamed.
    expect_true(all(vapply_1l(out$Locations, inherits, what = "ExportedLocation")))
    expect_named(out$Locations, NULL)
})

test_that("export.Text() validates set_uuid", {
    expect_error(export(txt, set_uuid = 1L))
    expect_snapshot(export(txt, set_uuid = 1L), error = TRUE)
})

test_that("export.Text() validates set_translations", {
    expect_error(export(txt, set_translations = 1L))
    expect_snapshot(export(txt, set_translations = 1L), error = TRUE)
})

test_that("export.Text() sets _Uuid as null if set_uuid is false", {
    out <- export(txt, FALSE)

    expect_in("_Uuid", names(out))
    expect_null(out$`_Uuid`)
})

test_that("export.Text() sets translations if set_translations is true", {
    out <- export(txt, set_translations = TRUE)

    # This test ignores call to strwrap and expects objects
    # to be equal because the underlying strings are short.
    expect_identical(out$Translations, list(
        es = "¡Hola Mundo!",
        fr = "Bonjour, monde!",
        ja = "こんにちは世界！"
    ))
})

test_that("export.Text() wraps source text and translations longer than 80 chars", {
    txt <- text(
        en = normalize("
            Lorem Ipsum is simply dummy text of the printing and typesetting
            industry. Lorem Ipsum has been the industry's standard dummy text
            ever since the 1500s, when an unknown printer took a galley of type
            and scrambled it to make a type specimen book. It has survived not
            only five centuries, but also the leap into electronic typesetting,
            remaining essentially unchanged. It was popularised in the 1960s with
            the release of Letraset sheets containing Lorem Ipsum passages, and
            more recently with desktop publishing software like Aldus PageMaker
            including versions of Lorem Ipsum."),
        fr = normalize("
            Le Lorem Ipsum est simplement du faux texte employé dans la
            composition et la mise en page avant impression. Le Lorem Ipsum
            est le faux texte standard de l'imprimerie depuis les années 1500,
            quand un imprimeur anonyme assembla ensemble des morceaux de texte
            pour réaliser un livre spécimen de polices de texte. Il n'a pas
            fait que survivre cinq siècles, mais s'est aussi adapté à la
            bureautique informatique, sans que son contenu n'en soit modifié.
            Il a été popularisé dans les années 1960 grâce à la vente de
            feuilles Letraset contenant des passages du Lorem Ipsum, et, plus
            récemment, par son inclusion dans des applications de mise en page
            de texte, comme Aldus PageMaker."))

    out <- export(txt, set_translations = TRUE)

    expect_type(out$`Source Text`, "character")
    expect_true(length(out$`Source Text`) > 1L)
    expect_type(out$`Translations`$fr, "character")
    expect_true(length(out$`Translations`$fr) > 1L)
})

test_that("export.Text() only sets fields Hash, Source Language, and Source Text if x has a set source_lang", {
    out <- export(Text$new())

    expect_length(out, 7L)
    expect_null(out$Hash)
    expect_null(out$`Source Language`)
    expect_null(out$`Source Text`)
})


# export.Location() ------------------------------------------------------------


test_that("export.Location() returns a named list of S3 class ExportedLocation", {
    out <- export(loc)

    expect_s3_class(out, "ExportedLocation")
    expect_identical(attr(out, "tag"), "ExportedLocation")
    expect_type(out, "list")
    expect_length(out, 3L)

    # _Uuid is random and cannot be compared to a reference value.
    expect_type(out$`_Uuid`, "character")
    expect_length(out$`_Uuid`, 1L)

    expect_identical(out$Path, loc$path)
    expect_identical(out$Ranges, c(
        "line  1, column  22 @ line   10, column 1",
        "line 11, column 222 @ line 3333, column 4"))
})

test_that("export.Location() validates set_uuid", {
    expect_error(export(loc, set_uuid = 1L))
    expect_snapshot(export(loc, set_uuid = 1L), error = TRUE)
})

test_that("export.Location() sets _Uuid as null if set_uuid is false", {
    out <- export(loc, FALSE)

    expect_in("_Uuid", names(out))
    expect_null(out$`_Uuid`)
})

test_that("export.Location() passes ... to .location_format_range()", {
    out <- export(loc, how = "shorter")

    expect_identical(out$Ranges, c(
        " 1, 22 @   10,1",
        "11,222 @ 3333,4"))
})
