test_that(".onLoad() returns null invisibly", {
    expect_null(transltr:::.onLoad(.Library, "transltr"))
    expect_invisible(transltr:::.onLoad(.Library, "transltr"))
})

test_that(".onLoad() sets options", {
    # Options are removed and restored afterwards.
    withr::local_options(transltr.path    = NULL)
    withr::local_options(transltr.verbose = NULL)
    transltr:::.onLoad(.Library, "transltr")

    expect_identical(
        getOption("transltr.path"),
        file.path("inst", "transltr", "_translator.yml"))
    expect_true(getOption("transltr.verbose"))
})

test_that(".onLoad() does not set options if they are already defined", {
    withr::local_options(transltr.path    = "_translator.yml")
    withr::local_options(transltr.verbose = FALSE)
    transltr:::.onLoad(.Library, "transltr")

    expect_identical(getOption("transltr.path"), "_translator.yml")
    expect_false(getOption("transltr.verbose"))
})
