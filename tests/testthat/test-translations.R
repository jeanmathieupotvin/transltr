mock_template_v1_path <- get_mock_path("tsf-v1", "md")


# read_translations() ----------------------------------------------------------


# TODO: update unit tests once class Translator is implemented. There is not
# much to test because everything is already covered by unit tests of lower-
# level mechanisms.

test_that("read_translations() returns a Translator object", {
    skip("class Translator is not yet implemented")
})

test_that("read_translations() properly parses translations source files (version 1)", {
    out <- read_translations(mock_template_v1_path)

    expect_type(out, "list")
    expect_length(out, 2L)
})


# write_translations() ---------------------------------------------------------


test_that("write_translations() returns a not yet implemented error", {
    expect_error(write_translations())
})

