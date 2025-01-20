test_that("%??% works", {
    expect_null(NULL %??% NULL)
    expect_identical(NULL %??% 1L, 1L)
    expect_identical(1L %??% NULL, 1L)
})
