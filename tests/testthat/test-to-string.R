v <- c(1L, 2L, 3L)


# to_string() ------------------------------------------------------------------


test_that("to_string() works", {
    expect_type(to_string(1L), "character")
    expect_length(to_string(1L), 1L)
})


# to_string.default() ----------------------------------------------------------


test_that("to_string.default() returns a character", {
    test_out <- to_string(v)

    expect_type(test_out, "character")
    expect_length(test_out, 1L)
})

test_that("to_string.default() does not quote values by default", {
    expect_identical(to_string(v), "1, 2, or 3")
    expect_identical(to_string(v, quote_values = TRUE), "'1', '2', or '3'")

    expect_snapshot(to_string(c(1L, 2L, 3L)))
    expect_snapshot(to_string(c(1L, 2L, 3L), quote_values = TRUE))
})

test_that("to_string.default() uses last separator", {
    expect_identical(to_string(v, last_sep = " @@@ "), "1, 2 @@@ 3")
})

test_that("to_string.default() treats scalar values as expected", {
    expect_identical(to_string(integer()), character())
    expect_identical(to_string(integer(1L)), "0")
})
