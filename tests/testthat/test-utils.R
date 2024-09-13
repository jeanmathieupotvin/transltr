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


# strip_chars() ----------------------------------------------------------------


test_that("strip_chars() returns a character vector", {
    expect_type(strip_chars(), "character")
    expect_length(strip_chars(), 0L)
})

test_that("strip_chars() validates argument x", {
    expect_no_error(strip_chars(character(0L)))
    expect_error(strip_chars(1L))
    expect_snapshot(strip_chars(1L), error = TRUE)
})

test_that("strip_chars() validates argument chars", {
    expect_no_error(strip_chars(chars = character(0L)))
    expect_error(strip_chars(chars = 1L))
    expect_error(strip_chars("a", "aa"))
    expect_snapshot(strip_chars(chars = 1L), error = TRUE)
    expect_snapshot(strip_chars("a", "aa"),  error = TRUE)
})

test_that("strip_chars() returns argument x if it or argument chars is empty", {
    x <- c("a", "b", "c")

    expect_identical(strip_chars(x), x)
    expect_identical(strip_chars(chars = x), character(0L))
})

test_that("strip_chars() removes chars from x appropriately", {
    x <- c("{{ a }} ", "`bbb`", "##  c", "d", "")
    expected <- c("a", "bbb", "c", "d", "")

    expect_identical(strip_chars(x, c("{", "}", "`", "#", " ")), expected)
})
