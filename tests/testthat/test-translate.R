# translate() ------------------------------------------------------------------


test_that("translate() returns a character string", {
    withr::defer(translator_set(NULL, "test"))
    translator_set(
        x     = translator(block("en", en = "Hello!", es = "¡Hola!")),
        scope = "test")

    out <- translate("Hello!", key = "es", scope = "test")

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
    # function definition in a package. This function calls
    # translator_set() and translate(), which infer scope from
    # the underlying namespace (as expected). Since it returns
    # a matching id for testing purposes.
    out_global <- evalq(envir = globalenv(), \() {
        on.exit(translator_set(NULL))
        translator_set(translator(block("en", en = "Hello!", fr = "Bonjour!")))
        return(translate("Hello!", key = "fr"))
    })()
    out_stats <- evalq(envir = asNamespace("stats"), \() {
        on.exit(translator_set(NULL))
        translator_set(translator(block("en", en = "Hello!", es = "¡Hola!")))
        return(translate("Hello!", key = "es"))
    })()
    out_utils <- evalq(envir = asNamespace("utils"), \() {
        on.exit(translator_set(NULL))
        translator_set(translator(block("en", en = "Hello!", jp = "こんにちは！")))
        return(translate("Hello!", key = "jp"))
    })()

    expect_identical(out_global, "Bonjour!")
    expect_identical(out_stats,  "¡Hola!")
    expect_identical(out_utils,  "こんにちは！")
})

test_that("translate() throws an error if implicit scope has no set translator object", {
    expect_error({
        evalq(envir = globalenv(), \() translate("Hello, world!", key = "fr"))()
    })
    expect_snapshot(error = TRUE, {
        evalq(envir = globalenv(), \() translate("Hello, world!", key = "fr"))()
    })
})
