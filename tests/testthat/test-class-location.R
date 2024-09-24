# path does not need to exist. Location objects
# may refer non-existent files and/or ranges.
path <- "tests/testthat/my-test-file"

# We use extreme dummy ranges to test whether
# values are appropriately padded or not by format().
loc <- location(path, c(1L, 11L), c(22L, 222L), c(10L, 3333L), c(1L, 4L))


# location() -------------------------------------------------------------------


test_that("location() returns a S3 object of class Location", {
    expect_s3_class(loc, "Location")
    expect_type(loc, "list")
    expect_length(loc, 5L)
    expect_identical(loc$path, path)
    expect_identical(loc$line1, c(1L, 11L))
    expect_identical(loc$col1,  c(22L, 222L))
    expect_identical(loc$line2, c(10L, 3333L))
    expect_identical(loc$col2,  c(1L, 4L))
})

test_that("location() validates argument path", {
    expect_error(location(1L))
    expect_snapshot(location(1L), error = TRUE)
})

test_that("location() validates argument line1", {
    expect_error(location(line1 = ""))
    expect_error(location(line1 = 0L))
    expect_snapshot(location(line1 = ""), error = TRUE)
    expect_snapshot(location(line1 = 0L), error = TRUE)
})

test_that("location() validates argument col1", {
    expect_error(location(col1 = ""))
    expect_error(location(col1 = 0L))
    expect_snapshot(location(col1 = ""), error = TRUE)
    expect_snapshot(location(col1 = 0L), error = TRUE)
})

test_that("location() validates argument line2", {
    expect_error(location(line2 = ""))
    expect_error(location(line2 = 0L))
    expect_snapshot(location(line2 = ""), error = TRUE)
    expect_snapshot(location(line2 = 0L), error = TRUE)
})

test_that("location() validates argument col2", {
    expect_error(location(col2 = ""))
    expect_error(location(col2 = 0L))
    expect_snapshot(location(col2 = ""), error = TRUE)
    expect_snapshot(location(col2 = 0L), error = TRUE)
})

test_that("location() validates line1, col1, line2, and col2 have the same length", {
    expect_error(location(line1 = c(1L, 2L)))
    expect_snapshot(location(line1 = c(1L, 2L)), error = TRUE)
})


# is_location() ----------------------------------------------------------------


test_that("is_location() returns a logical", {
    expect_true(is_location(location()))
    expect_false(is_location(1L))
})


# format.Location() ------------------------------------------------------------


test_that("format() returns a character", {
    # This test block is a little bit
    # fragile, but hardcoding expected
    # values is much more simpler.
    fmt_loc <- format(loc)

    expect_type(fmt_loc, "character")
    expect_length(fmt_loc, 4L)
    expect_identical(fmt_loc[[1L]], "<Location>")
    expect_identical(fmt_loc[[2L]], "  'tests/testthat/my-test-file':")
    expect_identical(fmt_loc[[3L]], "    - line  1, column  22 @ line   10, column 1")
    expect_identical(fmt_loc[[4L]], "    - line 11, column 222 @ line 3333, column 4")
})


# print.Location() -------------------------------------------------------------


test_that("print() returns its argument invisibly", {
    withr::local_output_sink(tempfile())
    expect_invisible(print(loc))
    expect_identical(print(loc), loc)
})

test_that("print() works", {
    expect_output(print(loc), "^<Location>")
    expect_snapshot(print(loc))
})
