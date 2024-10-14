v1 <- c("a", "bb", "ccc")
v2 <- c(
    "A very short line",
    "A line that contains 34 characters",
    "Another line that contains 40 characters")


# str_strip_empty() ------------------------------------------------------------


test_that("str_strip_empty() returns a character", {
    expect_type(str_strip_empty(), "character")
    expect_length(str_strip_empty(), 0L)
})

test_that("str_strip_empty() validates x", {
    expect_error(str_strip_empty(1L))
    expect_snapshot(str_strip_empty(1L), error = TRUE)
})

test_that("str_strip_empty() validates which", {
    expect_error(str_strip_empty(which = "error"))
    expect_snapshot(str_strip_empty(which = "error"), error = TRUE)
})

test_that("str_strip_empty() removes leading and trailing empty elements", {
    v <- c("", "", "a", "b", "", "c", "")

    expect_identical(str_strip_empty(v),             c("a", "b", "", "c"))
    expect_identical(str_strip_empty(v, "leading"),  c("a", "b", "", "c", ""))
    expect_identical(str_strip_empty(v, "trailing"), c("", "", "a", "b", "", "c"))
})

test_that("str_strip_empty() returns an empty character if it is empty", {
    expect_identical(str_strip_empty(character(0L)), character(0L))
})

test_that("str_strip_empty() returns an empty character if all elements are empty strings", {
    expect_identical(str_strip_empty(""), character(0L))
    expect_identical(str_strip_empty(c("", "", "")), character(0L))
})


# str_left_pad() ---------------------------------------------------------------


test_that("str_left_pad() returns a character", {
    expect_type(str_left_pad(), "character")
    expect_length(str_left_pad(), 0L)

    expect_type(str_left_pad(v1), "character")
    expect_length(str_left_pad(v1), length(v1))
})

test_that("str_left_pad() validates x", {
    expect_error(str_left_pad(1L))
    expect_snapshot(str_left_pad(1L), error = TRUE)
})

test_that("str_left_pad() validates pad", {
    expect_error(str_left_pad(pad = 1L))
    expect_error(str_left_pad(v1, pad = "aa"))
    expect_snapshot(str_left_pad(pad = 1L),      error = TRUE)
    expect_snapshot(str_left_pad(v1, pad = "aa"), error = TRUE)
})

test_that("str_left_pad() validates len", {
    expect_error(str_left_pad(v1, len = "1"))
    expect_error(str_left_pad(v1, len = -1L))
    expect_snapshot(str_left_pad(v1, len = "1"), error = TRUE)
    expect_snapshot(str_left_pad(v1, len = -1L), error = TRUE)
})

test_that("str_left_pad() sets len if left as null", {
    expect_true(all(nchar(str_left_pad(v1) == 3L)))
})

test_that("str_left_pad() adds padding to elements of x as expected ", {
    expect_identical(str_left_pad(v1, 0L), v1)
    expect_identical(str_left_pad(v1, 1L), v1)
    expect_identical(str_left_pad(v1, NULL), c("  a",   " bb",   "ccc"))
    expect_identical(str_left_pad(v1, 4L),   c("   a",  "  bb",  " ccc"))
    expect_identical(str_left_pad(v1, 5L),   c("    a", "   bb", "  ccc"))
})


# str_trim() -------------------------------------------------------------------


test_that("str_trim() returns a character", {
    expect_type(str_trim(), "character")
    expect_length(str_trim(), 0L)
    expect_identical(str_trim(v2), v2)
})

test_that("str_trim() validates x", {
    expect_error(str_trim(1L))
    expect_snapshot(str_trim(1L), error = TRUE)
})

test_that("str_trim() validates len", {
    expect_error(str_trim(v2, "1"))
    expect_error(str_trim(v2, 2L))
    expect_snapshot(str_trim(v2, "1"), error = TRUE)
    expect_snapshot(str_trim(v2, 2L),  error = TRUE)
})

test_that("str_trim() trims strings as expected", {
    v_trim <- str_trim(v2, 20L)

    expect_true(all(nchar(v_trim) <= 20L))
    expect_identical(v_trim, c(
        "A very short line",
        "A line that conta...",
        "Another line that..."))
})


# str_sanitize() ---------------------------------------------------------------


test_that("str_sanitize() returns a character string", {
    expect_type(str_sanitize(v2), "character")
    expect_length(str_sanitize(v2), 1L)
})

test_that("str_sanitize() validates x", {
    expect_error(str_sanitize(1L))
    expect_snapshot(str_sanitize(1L), error = TRUE)
})

test_that("str_sanitize() validates concat", {
    expect_error(str_sanitize(v1, concat = 1L))
    expect_snapshot(str_sanitize(v1, concat = 1L), error = TRUE)
})

test_that("str_sanitize() replaces space characters as expected", {
    v <- c(
        "A normal line",
        "\n\t A line that starts with spaces",
        "A line   with multiple  \t\t spaces")

    expect_identical(
        str_sanitize(v, concat = "; "),
        "A normal line; A line that starts with spaces; A line with multiple spaces")
})
