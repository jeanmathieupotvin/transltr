# translate() ------------------------------------------------------------------


test_that("translate() returns a character string", {
    withr::defer(translator_set(NULL, "test"))
    translator_set(
        x     = translator(block("en", en = "Hello!", es = "¡Hola!")),
        scope = "test")

    out <- translate("Hello!", lang = "es", scope = "test")

    expect_type(out, "character")
    expect_length(out, 1L)
})

test_that("translate() validates translator object it fetches for given scope", {
    expect_error(translate("Hello, world!", scope = "error"))
    expect_snapshot(translate("Hello, world!", scope = "error"), error = TRUE)
})

test_that("translate() works with implicit scopes", {
    # To test further scopes stemming from namespaces and named
    # environments, we use evalq(). Each call below simulates a
    # function in a package. It calls functions that infer scope
    # from the underlying namespace. block() is not exported, and
    # is not on the search path in normal circumstances. We must
    # fetch it explicitly from the package.
    out_global <- evalq(envir = globalenv(), \() {
        on.exit(translator_set(NULL))
        blk <- transltr::block(en = "Hello!", fr = "Bonjour!")
        translator_set(translator(blk))
        return(translate("Hello!", lang = "fr"))
    })()
    out_stats <- evalq(envir = asNamespace("stats"), \() {
        on.exit(translator_set(NULL))
        blk <- transltr::block(en = "Hello!", es = "¡Hola!")
        translator_set(translator(blk))
        return(translate("Hello!", lang = "es"))
    })()
    out_utils <- evalq(envir = asNamespace("utils"), \() {
        on.exit(translator_set(NULL))
        blk <- transltr::block(en = "Hello!", ja = "こんにちは！")
        translator_set(translator(blk))
        return(translate("Hello!", lang = "ja"))
    })()

    expect_identical(out_global, "Bonjour!")
    expect_identical(out_stats,  "¡Hola!")
    expect_identical(out_utils,  "こんにちは！")
})

test_that("translate() throws an error if implicit scope has no set translator object", {
    expect_error({
        evalq(envir = globalenv(), \() translate("Hello, world!", lang = "fr"))()
    })
    expect_snapshot(error = TRUE, {
        evalq(envir = globalenv(), \() translate("Hello, world!", lang = "fr"))()
    })
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
