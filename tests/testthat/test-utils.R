# %??% -------------------------------------------------------------------------


test_that("nullish coalescing special operator %??% works", {
    expect_identical(NULL %??% 1L, 1L)
    expect_identical(1L %??% 2L,   1L)
})


# vapply_1l() ------------------------------------------------------------------


test_that("vapply_1l() returns a logical vector", {
    test_vec <- c(1L, 2L, 3L)
    test_out <- vapply_1l(test_vec, `>`, e2 = 0L)

    expect_type(test_out, "logical")
    expect_length(test_out, length(test_vec))
})


# vapply_1c() ------------------------------------------------------------------


test_that("vapply_1c() returns a character vector", {
    test_vec <- c(1L, 2L, 3L)
    test_out <- vapply_1c(test_vec, as.character)

    expect_type(test_out, "character")
    expect_length(test_out, length(test_vec))
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
