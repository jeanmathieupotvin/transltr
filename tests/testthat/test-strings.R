v1 <- c("a", "bb", "ccc")
v2 <- c(
    "A very short line",
    "A line that contains 34 characters",
    "Another line that contains 40 characters")


# strip_empty_strings() --------------------------------------------------------


test_that("strip_empty_strings() returns a character", {
    expect_type(strip_empty_strings(), "character")
    expect_length(strip_empty_strings(), 0L)
})

test_that("strip_empty_strings() validates x", {
    expect_error(strip_empty_strings(1L))
    expect_snapshot(strip_empty_strings(1L), error = TRUE)
})

test_that("strip_empty_strings() validates which", {
    expect_error(strip_empty_strings(which = "error"))
    expect_snapshot(strip_empty_strings(which = "error"), error = TRUE)
})

test_that("strip_empty_strings() removes leading and trailing empty elements", {
    v <- c("", "", "a", "b", "", "c", "")

    expect_identical(strip_empty_strings(v),             c("a", "b", "", "c"))
    expect_identical(strip_empty_strings(v, "leading"),  c("a", "b", "", "c", ""))
    expect_identical(strip_empty_strings(v, "trailing"), c("", "", "a", "b", "", "c"))
})

test_that("strip_empty_strings() returns an empty character if it is empty", {
    expect_identical(strip_empty_strings(character(0L)), character(0L))
})

test_that("strip_empty_strings() returns an empty character if all elements are empty strings", {
    expect_identical(strip_empty_strings(""), character(0L))
    expect_identical(strip_empty_strings(c("", "", "")), character(0L))
})


# left_pad_strings() -----------------------------------------------------------


test_that("left_pad_strings() returns a character", {
    expect_type(left_pad_strings(), "character")
    expect_length(left_pad_strings(), 0L)

    expect_type(left_pad_strings(v1), "character")
    expect_length(left_pad_strings(v1), length(v1))
})

test_that("left_pad_strings() validates x", {
    expect_error(left_pad_strings(1L))
    expect_snapshot(left_pad_strings(1L), error = TRUE)
})

test_that("left_pad_strings() validates pad", {
    expect_error(left_pad_strings(pad = 1L))
    expect_error(left_pad_strings(v1, pad = "aa"))
    expect_snapshot(left_pad_strings(pad = 1L),      error = TRUE)
    expect_snapshot(left_pad_strings(v1, pad = "aa"), error = TRUE)
})

test_that("left_pad_strings() validates len", {
    expect_error(left_pad_strings(v1, len = "1"))
    expect_error(left_pad_strings(v1, len = -1L))
    expect_snapshot(left_pad_strings(v1, len = "1"), error = TRUE)
    expect_snapshot(left_pad_strings(v1, len = -1L), error = TRUE)
})

test_that("left_pad_strings() sets len if left as null", {
    expect_true(all(nchar(left_pad_strings(v1) == 3L)))
})

test_that("left_pad_strings() adds padding to elements of x as expected ", {
    expect_identical(left_pad_strings(v1, 0L), v1)
    expect_identical(left_pad_strings(v1, 1L), v1)
    expect_identical(left_pad_strings(v1, NULL), c("  a",   " bb",   "ccc"))
    expect_identical(left_pad_strings(v1, 4L),   c("   a",  "  bb",  " ccc"))
    expect_identical(left_pad_strings(v1, 5L),   c("    a", "   bb", "  ccc"))
})


# trim_strings() ---------------------------------------------------------------


test_that("trim_strings() returns a character", {
    expect_type(trim_strings(), "character")
    expect_length(trim_strings(), 0L)
    expect_identical(trim_strings(v2), v2)
})

test_that("trim_strings() validates x", {
    expect_error(trim_strings(1L))
    expect_snapshot(trim_strings(1L), error = TRUE)
})

test_that("trim_strings() validates len", {
    expect_error(trim_strings(v2, "1"))
    expect_error(trim_strings(v2, 2L))
    expect_snapshot(trim_strings(v2, "1"), error = TRUE)
    expect_snapshot(trim_strings(v2, 2L),  error = TRUE)
})

test_that("trim_strings() trims strings as expected", {
    v_trim <- trim_strings(v2, 20L)

    expect_true(all(nchar(v_trim) <= 20L))
    expect_identical(v_trim, c(
        "A very short line",
        "A line that conta...",
        "Another line that..."))
})


# sanitize_strings() -----------------------------------------------------------


test_that("sanitize_strings() returns a character string", {
    expect_type(sanitize_strings(v2), "character")
    expect_length(sanitize_strings(v2), 1L)
})

test_that("sanitize_strings() validates x", {
    expect_error(sanitize_strings(1L))
    expect_snapshot(sanitize_strings(1L), error = TRUE)
})

test_that("sanitize_strings() validates concat", {
    expect_error(sanitize_strings(v1, concat = 1L))
    expect_snapshot(sanitize_strings(v1, concat = 1L), error = TRUE)
})

test_that("sanitize_strings() replaces space characters as expected", {
    v <- c(
        "A normal line",
        "\n\t A line that starts with spaces",
        "A line   with multiple  \t\t spaces")

    expect_identical(
        sanitize_strings(v, concat = "; "),
        "A normal line; A line that starts with spaces; A line with multiple spaces")
})
