test_that("isSingleChar() returns true for single character strings", {
    expect_true(isSingleChar(""))
    expect_true(isSingleChar("a"))
    expect_true(isSingleChar("aa"))
})

test_that("isSingleChar() returns false for non-single character strings", {
    expect_false(isSingleChar(1L))
    expect_false(isSingleChar(c("", "")))
    expect_false(isSingleChar(NA_character_))
})
