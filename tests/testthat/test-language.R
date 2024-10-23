withr::defer(language_set(NULL))


# language_set() ---------------------------------------------------------------


test_that("language_set() returns null invisibly", {
    expect_null(language_set())
    expect_invisible(language_set())

    expect_null(language_set(NULL))
    expect_invisible(language_set(NULL))
})

test_that("language_set() validates key", {
    expect_error(language_set(1L))
    expect_snapshot(language_set(1L), error = TRUE)
})

test_that("language_set() sets environment variable", {
    withr::defer(language_set(NULL))
    language_set("test-en")

    expect_identical(language_get(), "test-en")
})

test_that("language_set() unsets environment variable if key is null", {
    language_set("test-en")
    language_set(NULL)

    expect_identical(language_get(), "")
})

test_that("language_set() throws an error if it fails to set environment variable", {
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

test_that("language_set() throws an error if it fails to unset environment variable", {
    # Testing failure of Sys.unsetenv() is (almost)
    # impossible. To circumscribe this, we locally
    # set internal flag to TRUE to force an error.
    local_mocked_bindings(.__LGL_DEBUG_FLAG = TRUE)
    expect_error(language_set(NULL))
    expect_snapshot(error = TRUE, {
        ".__LGL_DEBUG_FLAG was set equal to TRUE to generate this error."
        language_set(NULL)
    })
})


# language_get() ---------------------------------------------------------------


test_that("language_get() returns a character string if set", {
    language_set("test-en")
    withr::defer(language_set(NULL))

    expect_identical(language_get(), "test-en")
})

test_that("language_get() returns null if not set or empty", {
    withr::with_envvar(list(TRANSLTR_LANGUAGE = NA_character_), {
        expect_identical(language_get(), "")
    })
    withr::with_envvar(list(TRANSLTR_LANGUAGE = ""), {
        expect_identical(language_get(), "")
    })
})
