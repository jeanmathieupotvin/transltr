language_source_set("en")
withr::defer(language_source_set(NULL))

tr <- translator(
    en = "English",
    es = "Español",
    text(en = "Hello!", es = "¡Hola!"))

test_that("it returns a character string", {
    out <- translate("Hello!", lang = "es", tr = tr)

    expect_type(out, "character")
    expect_length(out, 1L)
})

test_that("it validates tr", {
    expect_error(translate("Hello, world!", tr = 1L))
    expect_snapshot(translate("Hello, world!", tr = 1L), error = TRUE)
})
