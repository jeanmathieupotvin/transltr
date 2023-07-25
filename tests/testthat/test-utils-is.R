test_that("isSingleChar() returns true for valid character strings", {
    expect_true(isSingleChar(""))
    expect_true(isSingleChar("a"))
    expect_true(isSingleChar("aa"))
})

test_that("isSingleChar() returns false for invalid character strings", {
    expect_false(isSingleChar(1L))
    expect_false(isSingleChar(NA_character_))
})

test_that("isSingleChar() returns false for character with a length greater than 1", {
    expect_false(isSingleChar(c("", "")))
})

test_that("isSingleIntegerInRange() returns true for valid single integers", {
    expect_true(isSingleIntegerInRange(0L))
    expect_true(isSingleIntegerInRange(.Machine$integer.max))
    expect_true(isSingleIntegerInRange(-.Machine$integer.max))
})

test_that("isSingleIntegerInRange() returns false for invalid integers", {
    # All values below are double values.
    # Integers that overflow or underflow are
    # automatically coerced to double values
    # with a warning not required.
    expect_false(isSingleIntegerInRange(0.0))
    expect_false(isSingleIntegerInRange(suppressWarnings(.Machine$integer.max  + 1L)))
    expect_false(isSingleIntegerInRange(suppressWarnings(-.Machine$integer.max - 1L)))
    expect_false(isSingleIntegerInRange(NA_integer_))
})
