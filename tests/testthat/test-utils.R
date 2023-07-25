test_that("map() returns a list", {
    out <- map(`+`, c(1L, 2L), c(3L, 4L))

    expect_type(out, "list")
    expect_length(out, 2L)
    expect_identical(out, list(4L, 6L))
})

test_that("vapply1l() returns a logical", {
    out <- vapply1l(c(one = 1L, two = 2L), `>`, e2 = 0L)

    expect_type(out, "logical")
    expect_length(out, 2L)
    expect_null(names(out))
    expect_identical(out, c(TRUE, TRUE))
})

test_that("vapply1i() returns an integer", {
    out <- vapply1i(c(one = 1L, two = 2L), `+`, e2 = 1L)

    expect_type(out, "integer")
    expect_length(out, 2L)
    expect_null(names(out))
    expect_identical(out, c(2L, 3L))
})

test_that("vapply1c() returns a character", {
    out <- vapply1c(c(one = "1", two = "2"), paste0, "1")

    expect_type(out, "character")
    expect_length(out, 2L)
    expect_null(names(out))
    expect_identical(out, c("11", "21"))
})

test_that("%||% works", {
    expect_identical(NULL %||% 2L, 2L)
    expect_identical(1L   %||% 2L, 1L)
})
