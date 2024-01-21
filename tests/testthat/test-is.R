test_that("isSingleIntInRange() returns true for valid single integers", {
    expect_true(isSingleIntInRange(0L))
    expect_true(isSingleIntInRange(.Machine$integer.max))
    expect_true(isSingleIntInRange(-.Machine$integer.max))
})

test_that("isSingleIntInRange() returns false for invalid integers", {
    # Integers that overflow or underflow are
    # automatically coerced to double values
    # with a warning not required.
    maxInteger <- .Machine$integer.max

    expect_false(isSingleIntInRange(0.0))
    expect_false(isSingleIntInRange(suppressWarnings(maxInteger  + 1L)))
    expect_false(isSingleIntInRange(suppressWarnings(-maxInteger - 1L)))
    expect_false(isSingleIntInRange(NA_integer_))
})
