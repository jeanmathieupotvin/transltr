# Setup ------------------------------------------------------------------------


# Extract value from environment containing a single element.
first <- function(env = new.env()) {
    return(as.list(env)[[1L]])
}

implicitCall   <- str2lang('translate("hello, world!")')
explicitCall   <- str2lang('transltr::translate("hello, world!")')
mockExpression <- dget(file.path("_mocks", "expression-with-translate-calls.R"))


# Tests ------------------------------------------------------------------------


test_that("getTranslationsFromExpression() returns a sorted list", {
    # Note: there are 4 calls to translate() in mockExpression.
    out <- getTranslationsFromExpression(mockExpression)

    expect_type(out, "list")
    expect_length(out, 4L)
    expect_false(is.unsorted(names(out)))
    expect_snapshot(getTranslationsFromExpression(mockExpression))
})

test_that("getStringFromTranslateCall() returns env invisibly", {
    env <- new.env(parent = emptyenv())

    expect_invisible(getStringFromTranslateCall(implicitCall))
    expect_identical(getStringFromTranslateCall(implicitCall, env), env)
})

test_that("getStringFromTranslateCall() extracts strings from calls", {
    envForImplicit <- getStringFromTranslateCall(implicitCall)
    envForExplicit <- getStringFromTranslateCall(explicitCall)
    stringId       <- getStringId("hello, world!")

    expect_identical(envForImplicit[[stringId]]$string, "hello, world!")
    expect_identical(envForExplicit[[stringId]]$string, "hello, world!")
})

test_that("getStringFromTranslateCall() concatenates all extracted strings", {
    string <- 'translate("hello, ", "world!")' |>
        str2lang() |>
        getStringFromTranslateCall() |>
        first()

    expect_identical(string$string, "hello, world!")
})

test_that("getStringFromTranslateCall() skips named arguments matching formal arguments", {
    string <- 'translate(a = "hello, ", b = "world!", lang = "en")' |>
        str2lang() |>
        getStringFromTranslateCall() |>
        first()

    expect_identical(string$string, "hello, world!")
})

test_that("getStringFromTranslateCall() registers strings as lists with identifiers", {
    string <- first(getStringFromTranslateCall(implicitCall))

    expect_type(string, "list")
    expect_length(string, 2L)
    expect_identical(string$string, "hello, world!")
    expect_identical(string$id, getStringId(string$string))
})

test_that("getStringId() works", {
    expect_identical(getStringId(), "cae66941d9efbd404e4d88758ea67670")
})

test_that("isTranslateCall() identifies implicit calls to translate()", {
    expect_true(isTranslateCall(implicitCall))
    expect_false(isTranslateCall("not a call"))
    expect_false(isTranslateCall("fun()"))
})

test_that("isTranslateCall() identifies explicit calls to translate()", {
    expect_true(isTranslateCall(explicitCall))
    expect_false(isTranslateCall(str2lang("pkg::translate()")))
    expect_false(isTranslateCall(str2lang("transltr::fun()")))
})
