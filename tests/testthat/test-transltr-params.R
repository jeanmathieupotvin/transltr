# get_template_versions() ------------------------------------------------------


test_that("get_template_versions() returns expected values", {
    expect_identical(get_template_versions(), 1L)
    expect_snapshot(get_template_versions())
})


# get_hash_algorithms() --------------------------------------------------------


test_that("get_hash_algorithms() returns expected values", {
    expect_identical(get_hash_algorithms(), "blake2b")
    expect_snapshot(get_hash_algorithms())
})


# get_hash_length_range() ------------------------------------------------------


test_that("get_hash_length_range() returns expected values", {
    expect_identical(get_hash_length_range("blake2b"), c(min = 8L, max = 32L))
    expect_snapshot(get_hash_length_range("blake2b"))
})

test_that("get_hash_length_range() validates hash_algorith,", {
    expect_error(get_hash_length_range("error"))
    expect_snapshot(get_hash_length_range("error"), error = TRUE)
})


# get_generated_by() -----------------------------------------------------------


test_that("get_generated_by() returns expected value", {
    current_ver          <- as.character(utils::packageVersion("transltr"))
    expeted_generated_by <- sprintf("R package transltr %s", current_ver)

    expect_identical(get_generated_by(), expeted_generated_by)
    expect_snapshot(get_generated_by())
})


# get_generated_on() -----------------------------------------------------------


# See script tests/testthat/test-utc.R.
