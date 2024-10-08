# vapply_1l() ------------------------------------------------------------------


test_that("vapply_1l() returns a logical vector", {
    v <- c(1L, 2L, 3L)
    out <- vapply_1l(v, `>`, e2 = 0L)

    expect_type(out, "logical")
    expect_length(out, length(v))
    expect_named(v, NULL)
})


# vapply_1i() ------------------------------------------------------------------


test_that("vapply_1i() returns a logical vector", {
    v <- c(1L, 2L, 3L)
    out <- vapply_1i(v, `+`, e2 = 0L)

    expect_type(out, "integer")
    expect_length(out, length(v))
    expect_named(v, NULL)
})


# vapply_1c() ------------------------------------------------------------------


test_that("vapply_1c() returns a character vector", {
    v <- c(1L, 2L, 3L)
    out <- vapply_1c(v, as.character)

    expect_type(out, "character")
    expect_length(out, length(v))
    expect_named(v, NULL)
})


# stops() ----------------------------------------------------------------------


test_that("stops() does not return the call as part of the error message", {
    expect_error(stops())
    expect_snapshot(error = TRUE, {
        wrap_stops <- \() stops("this is an error message.")
        wrap_stops()
    })
})


# stopf() ----------------------------------------------------------------------


test_that("stopf() works", {
    expect_error(stopf())
    expect_snapshot(
        stopf("this '%s' becomes part of the error message.", "placeholder"),
        error = TRUE)
})


# split_ul() -------------------------------------------------------------------


test_that("split_ul() works", {
    x   <- c(1L, 2L, 2L, 3L, 3L, 3L)
    out <- split_ul(x, x)

    expect_identical(out, list(1L, c(2L, 2L), c(3L, 3L, 3L)))
    expect_named(out, NULL)
})


# strip_empty_strings() --------------------------------------------------------


test_that("strip_empty_strings() returns a character vector", {
    expect_type(strip_empty_strings(), "character")
    expect_length(strip_empty_strings(), 0L)
})

test_that("strip_empty_strings() validates argument x", {
    expect_error(strip_empty_strings(1L))
    expect_snapshot(strip_empty_strings(1L), error = TRUE)
})

test_that("strip_empty_strings() validates argument which", {
    expect_error(strip_empty_strings(which = "error"))
    expect_snapshot(strip_empty_strings(which = "error"), error = TRUE)
})

test_that("strip_empty_strings() removes leading and trailing empty elements", {
    chr_vec <- c("", "", "a", "b", "", "c", "")

    expect_identical(strip_empty_strings(chr_vec),             c("a", "b", "", "c"))
    expect_identical(strip_empty_strings(chr_vec, "leading"),  c("a", "b", "", "c", ""))
    expect_identical(strip_empty_strings(chr_vec, "trailing"), c("", "", "a", "b", "", "c"))
})

test_that("strip_empty_strings() returns empty character vector if it is empty", {
    expect_identical(strip_empty_strings(character(0L)), character(0L))
})

test_that("strip_empty_strings() returns empty character vector if all elements are empty strings", {
    expect_identical(strip_empty_strings(""), character(0L))
    expect_identical(strip_empty_strings(c("", "", "")), character(0L))
})


# `%??%` -----------------------------------------------------------------------


test_that("operator %??% works", {
    expect_null(NULL %??% NULL)
    expect_identical(NULL %??% 1L, 1L)
    expect_identical(1L %??% NULL, 1L)
})
