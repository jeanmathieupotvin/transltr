test_that(".onLoad() registers options of the package", {
    # Options are removed and restored afterwards.
    withr::local_options(transltr.default.path = NULL)
    transltr:::.onLoad(.Library, "transltr")

    expect_identical(
        getOption("transltr.default.path"),
        file.path("inst", "transltr", "_translator.yml"))
})
