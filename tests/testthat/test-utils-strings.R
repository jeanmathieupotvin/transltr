v1 <- c(1L, 2L, 3L)
v2 <- c(
    "A very short line",
    "A line that contains 34 characters",
    "Another line that contains 40 characters")

# str_to() ---------------------------------------------------------------------

test_that("str_to() works", {
    expect_type(str_to(1L), "character")
    expect_length(str_to(1L), 1L)
})

# str_to.default() -------------------------------------------------------------

test_that("str_to.default() returns a character", {
    test_out <- str_to(v1)

    expect_type(test_out, "character")
    expect_length(test_out, 1L)
})

test_that("str_to.default() does not quote values by default", {
    expect_identical(str_to(v1), "1, 2, or 3")
    expect_identical(str_to(v1, quote_values = TRUE), "'1', '2', or '3'")

    expect_snapshot(str_to(c(1L, 2L, 3L)))
    expect_snapshot(str_to(c(1L, 2L, 3L), quote_values = TRUE))
})

test_that("str_to.default() uses last separator", {
    expect_identical(str_to(v1, last_sep = " @@@ "), "1, 2 @@@ 3")
})

test_that("str_to.default() treats scalar values as expected", {
    expect_identical(str_to(integer()), character())
    expect_identical(str_to(integer(1L)), "0")
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

test_that("str_trim() validates width", {
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

# str_wrap() -------------------------------------------------------------------

test_that("str_wrap() returns a character string", {
    expect_type(str_wrap(), "character")
    expect_length(str_wrap(), 1L)
    expect_length(str_wrap(as.character(v1)), 1L)
    expect_identical(
        str_wrap(v2),
        "A very short line\nA line that contains 34 characters\nAnother line that contains 40 characters")
})

test_that("str_wrap() validates x", {
    expect_error(str_wrap(1L))
    expect_snapshot(str_wrap(1L), error = TRUE)
})

test_that("str_wrap() validates width", {
    expect_error(str_wrap(v2, "1"))
    expect_error(str_wrap(v2, 0L))
    expect_snapshot(str_wrap(v2, "1"), error = TRUE)
    expect_snapshot(str_wrap(v2, 0L),  error = TRUE)
})

test_that("str_wrap() wraps lines", {
    expect_identical(
        str_wrap("A very short line", width = 4L),
        "A\nvery\nshort\nline")
    expect_identical(
        str_wrap(v2, width = 20L),
        "A very short line\nA line that\ncontains 34\ncharacters\nAnother line that\ncontains 40\ncharacters")
})
