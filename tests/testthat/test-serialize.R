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


# export.Location() ------------------------------------------------------------


test_that("export.Location() returns a named list of S3 class ExportedLocation", {
    out <- export(loc)

    expect_s3_class(out, "ExportedLocation")
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
