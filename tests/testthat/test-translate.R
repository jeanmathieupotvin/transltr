language_source_set("en")
withr::defer(language_source_set(NULL))

tr <- translator(
    en = "English",
    es = "Español",
    text(en = "Hello!", es = "¡Hola!"))


# translate() ------------------------------------------------------------------


test_that("translate() returns a character string", {
    out <- translate("Hello!", lang = "es", tr = tr)

    expect_type(out, "character")
    expect_length(out, 1L)
})

test_that("translate() validates tr", {
    expect_error(translate("Hello, world!", tr = 1L))
    expect_snapshot(translate("Hello, world!", tr = 1L), error = TRUE)
})


# is_translate_call() ----------------------------------------------------------


test_that("is_translate_call() returns a logical", {
    expect_true(is_translate_call(str2lang('translate("test")'), .strict = FALSE))
    expect_false(is_translate_call(str2lang('translate("test")')))
})

test_that("is_translate_call() handles typical use cases", {
    expect_true(is_translate_call(str2lang('translate("test")'), .strict = FALSE))
    expect_false(is_translate_call(str2lang('translate("test")')))
    expect_true(is_translate_call(str2lang('transltr::translate("test")')))
})

test_that("is_translate_call() handles quotes", {
    expect_true(is_translate_call(str2lang('"translate"("test")'), .strict = FALSE))
    expect_false(is_translate_call(str2lang('"translate"("test")')))
    expect_true(is_translate_call(str2lang('transltr::"translate"("test")')))
    expect_true(is_translate_call(str2lang('"transltr"::translate("test")')))
})

test_that("is_translate_call() handles backticks", {
    expect_true(is_translate_call(str2lang('`translate`("test")'), .strict = FALSE))
    expect_false(is_translate_call(str2lang('`translate`("test")')))
    expect_true(is_translate_call(str2lang('transltr::`translate`("test")')))
    expect_true(is_translate_call(str2lang('`transltr`::translate("test")')))
})

test_that("is_translate_call() handles quotes and backticks", {
    expect_true(is_translate_call(str2lang('"transltr"::`translate`("test")')))
    expect_true(is_translate_call(str2lang('`transltr`::"translate"("test")')))
})
