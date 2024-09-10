# test_file does not need to exist. Location objects
# may refer non-existent source files and/or ranges.
test_file <- "tests/testthat/my-test-file"
test_loc  <- location(test_file, 1L, 2L, 3L, 4L)


# location() -------------------------------------------------------------------


test_that("location() returns a S3 object of class Location", {
    expect_s3_class(test_loc, "Location")
    expect_type(test_loc, "list")
    expect_length(test_loc, 5L)
    expect_identical(test_loc$path, test_file)
    expect_identical(test_loc$line1, 1L)
    expect_identical(test_loc$col1,  2L)
    expect_identical(test_loc$line2, 3L)
    expect_identical(test_loc$col2,  4L)
})

test_that("location() validates argument path", {
    expect_error(location(1L))
    expect_snapshot(location(1L), error = TRUE)
})

test_that("location() validates argument line1", {
    expect_error(location(line1 = ""))
    expect_snapshot(location(line1 = ""), error = TRUE)
})

test_that("location() validates argument col1", {
    expect_error(location(col1 = ""))
    expect_snapshot(location(col1 = ""), error = TRUE)
})

test_that("location() validates argument line2", {
    expect_error(location(line2 = ""))
    expect_snapshot(location(line2 = ""), error = TRUE)
})

test_that("location() validates argument col2", {
    expect_error(location(col2 = ""))
    expect_snapshot(location(col2 = ""), error = TRUE)
})


# is_location() ----------------------------------------------------------------


test_that("is_location() returns a logical", {
    expect_true(is_location(location()))
    expect_false(is_location(1L))
})


# format.Location() ------------------------------------------------------------


test_that("format() returns a character string", {
    expect_identical(
        format(test_loc),
        "tests/testthat/my-test-file: ln 1, col 2 @ ln 3, col 4")
})


# print.Location() -------------------------------------------------------------


test_that("print() returns its argument invisibly", {
    withr::local_output_sink(tempfile())
    expect_invisible(print(test_loc))
    expect_identical(print(test_loc), test_loc)
})

test_that("print() works", {
    expect_output(print(test_loc), "^<Location>")
    expect_snapshot(print(test_loc))
})
