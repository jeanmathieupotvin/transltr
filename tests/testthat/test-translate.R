test_that("translate() throws an error until it is implemented", {
    expect_error(translate())
    expect_snapshot_error(translate())
})

test_that("isCallToTranslate() identifies calls to translate()", {
    expect_true(isCallToTranslate(quote(translate())))
    expect_true(isCallToTranslate(quote(`translate`())))
    expect_true(isCallToTranslate(quote("translate"())))
    expect_true(isCallToTranslate(quote(transltr::translate())))
    expect_true(isCallToTranslate(quote(transltr::`translate`())))
    expect_true(isCallToTranslate(quote(transltr::"translate"())))
})

test_that("isCallToTranslate() rejects invalid calls and non-call objects", {
    expect_false(isCallToTranslate("not a call"))
    expect_false(isCallToTranslate(quote(base::translate())))
    expect_false(isCallToTranslate(quote(transltr::translater())))
})

test_that("doEvaluateTranslateDefaultArgs() throws an error if translate() has no argument", {
    with_mocked_bindings(translate = \() { }, code = {
        expect_error(doEvaluateTranslateDefaultArgs())
        expect_snapshot_error(doEvaluateTranslateDefaultArgs())
    })
})

test_that("doEvaluateTranslateDefaultArgs() drops dot dot dot from output", {
    with_mocked_bindings(translate = \(..., a = "", b = "") { }, code = {
        expect_named(doEvaluateTranslateDefaultArgs(), c("a", "b"))
    })
})


# S3 ---------------------------------------------------------------------------


test_that("newTranslateCall() works", {
    skip("s3 generic covered by test cases of its methods")
})

test_that("newTranslateCall.call() returns a TranslateCall object", {
    expect_s3_class(
        newTranslateCall(quote(translate())),
        class = c("TranslateCall", "call"),
        exact = TRUE)
})

test_that("newTranslateCall.default() returns a TranslateCall object", {
    expect_s3_class(
        newTranslateCall(),
        class = c("TranslateCall", "call"),
        exact = TRUE)
})

test_that("newTranslateCall.default() constructs appropriate calls", {
    # We care only about the call here.

    expect_identical(
        newTranslateCall(),
        newTranslateCall(quote(translate())))
    expect_identical(
        newTranslateCall("hello", "world!"),
        newTranslateCall(quote(translate("hello", "world!"))))
    expect_identical(
        newTranslateCall("hello", "world!", concat = ", "),
        newTranslateCall(quote(translate("hello", "world!", concat = ", "))))
    expect_identical(
        newTranslateCall("world!", x = "hello", lang = "fr"),
        newTranslateCall(quote(translate("hello", "world!", lang = "fr"))))
})

test_that("as.TranslatableString.TranslateCall() returns a TranslatableString", {
    # This test case also covers S3 generic
    # as.TranslatableString() dispatch method.
    expect_s3_class(
        as.TranslatableString(newTranslateCall()),
        class = c("TranslatableString", "R6"),
        exact = TRUE)
})

test_that("as.TranslatableString.TranslateCall() processes dots appropriately", {
    # This test case checks if elements passed to ... of
    # translate() are properly evaluated before coercion.
    hello <- \() { return("hello")  }
    world <- \() { return("world!") }

    translateCall      <- newTranslateCall(hello(), world(), concat = ", ")
    translatableString <- as.TranslatableString(translateCall)

    expect_identical(translatableString$string, "hello, world!")
})

test_that("as.TranslatableString.TranslateCall() uses concat argument of call", {
    translateCall      <- newTranslateCall("hello", "world!", concat = ", ")
    translatableString <- as.TranslatableString(translateCall)

    expect_identical(translatableString$string, "hello, world!")
})
