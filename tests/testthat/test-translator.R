#' Testing translator_*() functions
#'
#' It is hard to completely decouple unit tests of [translator_set()] and
#' [translator_get()]. Since these two functions are very tightly coupled,
#' their test blocks are also closely related by design. Each test block
#' attempts to focus on one of them, but uses the other one to either get
#' or set values.
#'
#' It is recommended to have a strong understanding of the call stack before
#' reviewing [translator_set()] and [translator_get()]. The documentation of
#' [sys.function()] is useful. The idea of using [evalq()] below was taken
#' from the documentation of [gettext()].
#'
#' When leaving `scope` as `NULL`, [on.exit()] should be used instead of
#' [withr::defer()]. This is because the latter registers a callback function
#' with no namespace, and therefore, [translator_scope()] defaults to 'global'.
#' It is safer to sepcify a scope when using [withr::defer()]. This is one of
#' the potential improvements that could be introduced for [translator_scope()]
#' in the future.
NULL


# Ensure that the cache is empty.
cache <- .__translators_cache__
rm(list = ls(envir = cache, all.names = TRUE), pos = cache)


# translator_set() -------------------------------------------------------------


test_that("translator_set() returns null invisibly", {
    expect_null(translator_set())
    expect_null(translator_set(NULL))
})

test_that("translator_set() validates x", {
    expect_error(translator_set(1L))
    expect_snapshot(translator_set(1L), error = TRUE)
})

test_that("translator_set() validates scope", {
    expect_error(translator_set(scope = 1L))
    expect_snapshot(translator_set(scope = 1L), error = TRUE)
})

test_that("translator_set() sets scope if null", {
    withr::defer(translator_set(NULL))
    translator_set(translator(id = "test-null-scope"))
    id <- translator_get()$id
    expect_identical(id, "test-null-scope")
})

test_that("translator_set() sets translator objects", {
    # To test further scopes stemming from namespaces and named
    # environments, we use evalq(). Each call below simulates a
    # function in a package. It calls translator_set(), which
    # infers scope from the underlying namespace.
    scope_global <- evalq(envir = globalenv(), \() {
        translator_set(translator(id = "global"))
        id <- translator_get()$id
        translator_set(NULL)
        return(id)
    })()

    scope_stats <- evalq(envir = asNamespace("stats"), \() {
        transltr::translator_set(transltr::translator(id = "stats"))
        id <- transltr::translator_get()$id
        transltr::translator_set(NULL)
        return(id)
    })()

    scope_utils <- evalq(envir = asNamespace("utils"), \() {
        transltr::translator_set(transltr::translator(id = "utils"))
        id <- transltr::translator_get()$id
        transltr::translator_set(NULL)
        return(id)
    })()

    translator_set(translator(id = "test-scope"), "test-scope")
    withr::defer(translator_set(NULL, "test-scope"))

    expect_identical(scope_global, "global")
    expect_identical(scope_stats,  "stats")
    expect_identical(scope_utils,  "utils")
    expect_identical(translator_get("test-scope")$id, "test-scope")
})

test_that("translator_set() unsets translator objects", {
    evalq(envir = globalenv(), \() {
        translator_set(translator(id = "global"))
        translator_set(NULL)
    })()
    evalq(envir = asNamespace("stats"), \() {
        transltr::translator_set(transltr::translator(id = "stats"))
        transltr::translator_set(NULL)
    })()
    evalq(envir = asNamespace("utils"), \() {
        transltr::translator_set(transltr::translator(id = "utils"))
        transltr::translator_set(NULL)
    })()

    expect_null(translator_get("global"))
    expect_null(translator_get("stats"))
    expect_null(translator_get("utils"))
})

test_that("translator_set() does not throw a condition when unsetting non-existent translator objects", {
    # uuid() is used to ensure no collision.
    expect_no_condition(translator_set(NULL, uuid()))
})


# translator_get() -------------------------------------------------------------


test_that("translator_get() returns a translator object from the cache", {
    withr::defer(translator_set(NULL))
    translator_set(scope = "global")
    expect_s3_class(translator_get("global"), "Translator")
})

test_that("translator_get() returns null is scope has no translator object", {
    expect_null(translator_get("test-scope"))
})

test_that("translator_get() validates scope", {
    expect_error(translator_get(1L))
    expect_snapshot(translator_get(1L), error = TRUE)
})

test_that("translator_get() sets scope if null", {
    withr::defer(translator_set(NULL))
    translator_set(translator(id = "test-null-scope"))
    id <- translator_get()$id
    expect_identical(id, "test-null-scope")
})


# translator_scopes() ----------------------------------------------------------


test_that("translator_scopes() returns a character string", {
    expect_type(translator_scopes(), "character")
})


# translator_scope() -----------------------------------------------------------


test_that("translator_scope() returns a character string", {
    expect_type(translator_scope(), "character")
    expect_length(translator_scope(), 1L)
})

test_that("translator_scope() returns expected scope", {
    # translator_scope() is not exported, and is not
    # on the search path in normal circumstances. We
    # must fetch it explicitly from the package.
    scope_global <- evalq(envir = globalenv(),          \() transltr:::translator_scope())()
    scope_stats  <- evalq(envir = asNamespace("stats"), \() transltr:::translator_scope())()
    scope_utils  <- evalq(envir = asNamespace("utils"), \() transltr:::translator_scope())()

    expect_identical(scope_global, "global")
    expect_identical(scope_stats,  "stats")
    expect_identical(scope_utils,  "utils")
})


# translator_scope_name() ------------------------------------------------------


test_that("translator_scope_name() returns a character string", {
    named_env <- new.env()
    attr(named_env, "name") <- "my-env"

    expect_identical(translator_scope_name(1L), "global")           # default
    expect_identical(translator_scope_name("test"), "test")         # character
    expect_identical(translator_scope_name(\() { }), "global")      # normal fun
    expect_identical(translator_scope_name(utc), "transltr")        # namespaced fun
    expect_identical(translator_scope_name(gettext), "base")        # internal fun
    expect_identical(translator_scope_name(is.integer), "global")   # primitive fun
    expect_identical(translator_scope_name(new.env()), "global")    # unnamed env
    expect_identical(translator_scope_name(named_env), "my-env")    # named env
    expect_identical(translator_scope_name(globalenv()), "global")  # global env
})


# translator_read() ------------------------------------------------------------


test_that("translator_read() returns a not yet implemented error", {
    expect_error(translator_read())
})


# translator_write() -----------------------------------------------------------


test_that("translator_write() returns a not yet implemented error", {
    expect_error(translator_write())
})


# Cache ------------------------------------------------------------------------


test_that("internal cache for translator objects is well defined", {
    expect_type(cache, "environment")
    expect_length(cache, 0L)
})
