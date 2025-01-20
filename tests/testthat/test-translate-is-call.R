test_that("it returns a logical", {
    expect_true(is_translate_call(str2lang('translate("test")'), strict = FALSE))
    expect_false(is_translate_call(str2lang('translate("test")')))
})

test_that("it handles typical use cases", {
    expect_true(is_translate_call(str2lang('translate("test")'), strict = FALSE))
    expect_false(is_translate_call(str2lang('translate("test")')))
    expect_true(is_translate_call(str2lang('transltr::translate("test")')))
})

test_that("it handles quotes", {
    expect_true(is_translate_call(str2lang('"translate"("test")'), strict = FALSE))
    expect_false(is_translate_call(str2lang('"translate"("test")')))
    expect_true(is_translate_call(str2lang('transltr::"translate"("test")')))
    expect_true(is_translate_call(str2lang('"transltr"::translate("test")')))
})

test_that("it handles backticks", {
    expect_true(is_translate_call(str2lang('`translate`("test")'), strict = FALSE))
    expect_false(is_translate_call(str2lang('`translate`("test")')))
    expect_true(is_translate_call(str2lang('transltr::`translate`("test")')))
    expect_true(is_translate_call(str2lang('`transltr`::translate("test")')))
})

test_that("it handles quotes and backticks", {
    expect_true(is_translate_call(str2lang('"transltr"::`translate`("test")')))
    expect_true(is_translate_call(str2lang('`transltr`::"translate"("test")')))
})
