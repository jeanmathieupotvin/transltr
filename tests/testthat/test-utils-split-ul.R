test_that("split_ul() works", {
    x   <- c(1L, 2L, 2L, 3L, 3L, 3L)
    out <- split_ul(x, x)

    expect_identical(out, list(1L, c(2L, 2L), c(3L, 3L, 3L)))
    expect_named(out, NULL)
})
