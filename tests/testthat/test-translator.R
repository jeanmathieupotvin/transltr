#' Testing [translator_set()] and [translator_get()]
#'
#' It is hard to completely decouple unit tests of [translator_set()] and
#' [translator_get()]. Test blocks are related by design because these two
#' functions are tightly coupled. Each test block focuses on one of them,
#' but uses the other to either get or set test values.
#'
#' It is recommended to have a strong understanding of the call stack before
#' reviewing these functions. The documentation of [gettext()] provides some
#' information on why [evalq()] is used below. Sadly, the documentation of
#' R's Native Language Support (NLS) is scattered, incomplete, vague, and did
#' not age super well.
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

test_that("translator_set() sets translator objects", {
    # To test further scopes stemming from namespaces and
    # named environments, we use evalq(). Each call below
    # simulates a function definition in a package. This
    # function calls translator_set(), which infers scope
    # from the underlying namespace (as expected). Since
    # it returns a matching id for testing purposes.
    fn_global <- evalq(envir = globalenv(), \() {
        translator_set(translator(id = "global"))
        id <- translator_get()$id
        translator_set(NULL)
        return(id)
    })

    fn_stats <- evalq(envir = asNamespace("stats"), \() {
        translator_set(translator(id = "stats"))
        id <- translator_get()$id
        translator_set(NULL)
        return(id)
    })

    fn_utils <- evalq(envir = asNamespace("utils"), \() {
        translator_set(translator(id = "utils"))
        id <- translator_get()$id
        translator_set(NULL)
        return(id)
    })

    expect_identical(fn_global(), "global")
    expect_identical(fn_stats(),  "stats")
    expect_identical(fn_utils(),  "utils")
})

test_that("translator_set() unsets translator objects", {
    fn_global <- evalq(envir = globalenv(), \() {
        translator_set(translator(id = "global"))
        translator_set(NULL)
    })()
    fn_stats <- evalq(envir = asNamespace("stats"), \() {
        translator_set(translator(id = "stats"))
        translator_set(NULL)
    })()

    expect_null(translator_get("global"))
    expect_null(translator_get("stats"))
})


# translator_get() -------------------------------------------------------------


test_that("translator_get() returns a translator object from the cache", {
    withr::defer(translator_set(NULL))
    translator_set(translator(id = "global"))
    expect_identical(translator_get("global")$id, "global")
})

test_that("translator_get() returns null is scope has no translator object", {
    expect_null(translator_get("test-scope"))
})


# translator_scope() -----------------------------------------------------------


test_that("translator_scope() returns a character string", {
    named_env <- new.env()
    attr(named_env, "name") <- "my-env"

    expect_identical(translator_scope(1L), "global")           # default
    expect_identical(translator_scope("test"), "test")         # character
    expect_identical(translator_scope(\() { }), "global")      # normal fun
    expect_identical(translator_scope(utc), "transltr")        # namespaced fun
    expect_identical(translator_scope(gettext), "base")        # internal fun
    expect_identical(translator_scope(is.integer), "global")   # primitive fun
    expect_identical(translator_scope(new.env()), "global")    # unnamed env
    expect_identical(translator_scope(named_env), "my-env")    # named env
    expect_identical(translator_scope(globalenv()), "global")  # global env
})


# Cache ------------------------------------------------------------------------


test_that("internal cache for translator objects is well defined", {
    expect_type(cache, "environment")
    expect_length(cache, 0L)
})
