v <- c(1L, 2L, 3L)


# vapply_1l() ------------------------------------------------------------------


test_that("vapply_1l() returns a logical vector", {
    out <- vapply_1l(v, `>`, e2 = 0L)

    expect_type(out, "logical")
    expect_length(out, length(v))
    expect_named(v, NULL)
})


# vapply_1i() ------------------------------------------------------------------


test_that("vapply_1i() returns a logical vector", {
    out <- vapply_1i(v, `+`, e2 = 0L)

    expect_type(out, "integer")
    expect_length(out, length(v))
    expect_named(v, NULL)
})


# vapply_1c() ------------------------------------------------------------------


test_that("vapply_1c() returns a character vector", {
    out <- vapply_1c(v, as.character)

    expect_type(out, "character")
    expect_length(out, length(v))
    expect_named(v, NULL)
})


# map() ------------------------------------------------------------------------


test_that("map() returns a list", {
    out <- map(`+`, c(1L, 2L), c(3L, 4L))

    expect_type(out, "list")
    expect_length(out, 2L)
    expect_identical(out, list(4L, 6L))
})
