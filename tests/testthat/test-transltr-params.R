# get_template_versions() ------------------------------------------------------


test_that("get_template_versions() returns expected values", {
    expect_identical(get_template_versions(), 1L)
    expect_snapshot(get_template_versions())
})


# get_hash_algorithms() --------------------------------------------------------


test_that("get_hash_algorithms() returns expected values", {
    expect_identical(get_hash_algorithms(), c("sha1", "utf8"))
    expect_snapshot(get_hash_algorithms())
})


# get_generated_by() -----------------------------------------------------------


test_that("get_generated_by() returns expected value", {
    ver <- as.character(utils::packageVersion("transltr"))
    exp <- sprintf("R package transltr %s", ver)

    expect_identical(get_generated_by(), exp)
    expect_snapshot(get_generated_by())
})


# get_generated_on() -----------------------------------------------------------


# See script tests/testthat/test-utc.R.
