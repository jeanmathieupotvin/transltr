withr::defer({
    language_set(NULL)
    language_source_set(NULL)
})

# language_set() ---------------------------------------------------------------

test_that("language_set() returns null invisibly", {
    expect_null(language_set())
    expect_invisible(language_set())

    expect_null(language_set(NULL))
    expect_invisible(language_set(NULL))
})

test_that("language_set() validates lang", {
    expect_error(language_set(1L))
    expect_snapshot(language_set(1L), error = TRUE)
})

test_that("language_set() sets env variable", {
    withr::defer(language_set(NULL))
    language_set("test")

    expect_identical(language_get(), "test")
})

test_that("language_set() unsets env variable if lang is null", {
    language_set("test-en")
    language_set(NULL)

    expect_identical(language_get(), "")
})

test_that("language_set() throws an error if it fails to set env variable", {
    # Testing failure of Sys.setenv() is (almost)
    # impossible. To circumscribe this, we locally
    # set internal flag to TRUE to force an error.
    local_mocked_bindings(.__LGL_DEBUG_FLAG = TRUE)

    expect_error(language_set("test-en"))
    expect_snapshot(error = TRUE, {
        ".__LGL_DEBUG_FLAG was set equal to TRUE to generate this error."
        language_set("test-en")
    })
})

test_that("language_set() throws an error if it fails to unset env variable", {
    # Testing failure of Sys.unsetenv() is (almost)
    # impossible. To circumscribe this, we locally
    # set internal flag to TRUE to force an error.
    local_mocked_bindings(.__LGL_DEBUG_FLAG = TRUE)

    expect_error(language_set(NULL))
    expect_snapshot(error = TRUE, {
        ".__LGL_DEBUG_FLAG was set equal to TRUE to generate this error."
        "Therefore, the language is empty in the error message because it"
        "was reset before the flag triggers."
        language_set(NULL)
    })
})

# language_get() ---------------------------------------------------------------

test_that("language_get() returns a character string", {
    withr::with_envvar(list(TRANSLTR_LANGUAGE = "test"), {
        expect_identical(language_get(), "test")
    })
    withr::with_envvar(list(TRANSLTR_LANGUAGE = NA_character_), {
        expect_identical(language_get(), "")
    })
    withr::with_envvar(list(TRANSLTR_LANGUAGE = ""), {
        expect_identical(language_get(), "")
    })
})

# language_source_set() --------------------------------------------------------

test_that("language_source_set() returns null invisibly", {
    expect_null(language_source_set())
    expect_invisible(language_source_set())

    expect_null(language_source_set(NULL))
    expect_invisible(language_source_set(NULL))
})

test_that("language_source_set() validates lang", {
    expect_error(language_source_set(1L))
    expect_snapshot(language_source_set(1L), error = TRUE)
})

test_that("language_source_set() sets env variable", {
    withr::defer(language_source_set(NULL))
    language_source_set("test-en")

    expect_identical(language_source_get(), "test-en")
})

test_that("language_source_set() unsets env variable if lang is null", {
    language_source_set(NULL)

    expect_identical(language_source_get(), "en")
})

test_that("language_source_set() throws an error if it fails to set env variable", {
    # Testing failure of Sys.setenv() is (almost)
    # impossible. To circumscribe this, we locally
    # set internal flag to TRUE to force an error.
    local_mocked_bindings(.__LGL_DEBUG_FLAG = TRUE)

    expect_error(language_source_set("test-en"))
    expect_snapshot(error = TRUE, {
        ".__LGL_DEBUG_FLAG was set equal to TRUE to generate this error."
        language_source_set("test-en")
    })
})

test_that("language_source_set() throws an error if it fails to unset env variable", {
    # Testing failure of Sys.unsetenv() is (almost)
    # impossible. To circumscribe this, we locally
    # set internal flag to TRUE to force an error.
    local_mocked_bindings(.__LGL_DEBUG_FLAG = TRUE)

    expect_error(language_source_set(NULL))
    expect_snapshot(error = TRUE, {
        ".__LGL_DEBUG_FLAG was set equal to TRUE to generate this error."
        "Therefore, the language is 'en' (the default) in the error message"
        "because it was reset before the flag triggers."
        language_source_set(NULL)
    })
})

# language_source_get() --------------------------------------------------------

test_that("language_source_get() returns a character string", {
    withr::with_envvar(list(TRANSLTR_SOURCE_LANGUAGE = "test"), {
        expect_identical(language_source_get(), "test")
    })
    withr::with_envvar(list(TRANSLTR_SOURCE_LANGUAGE = NA_character_), {
        expect_identical(language_source_get(), "en")
    })
    withr::with_envvar(list(TRANSLTR_SOURCE_LANGUAGE = ""), {
        expect_identical(language_source_get(), "en")
    })
})
