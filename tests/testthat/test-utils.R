test_that("%||% works", {
    expect_identical(NULL %||% 2L, 2L)
    expect_identical(1L   %||% 2L, 1L)
})
