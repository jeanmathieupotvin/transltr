path <- "tests/testthat/my-test-file"

# We use extreme dummy ranges to test whether
# values are appropriately padded or not by format().
loc1 <- location(path, 1L, 2L, 3L, 4L)
loc2 <- location(path, c(1L, 11L), c(22L, 222L), c(10L, 3333L), c(1L, 4L))

ranges <- c(
    "Ln  1, Col  22 @ Ln   10, Col 1",
    "Ln 11, Col 222 @ Ln 3333, Col 4")

# location() -------------------------------------------------------------------

test_that("location() returns a S3 object of class Location", {
    expect_s3_class(loc2, "Location")
    expect_type(loc2, "list")
    expect_length(loc2, 5L)
    expect_identical(loc2$path, path)
    expect_identical(loc2$line1, c(1L, 11L))
    expect_identical(loc2$col1,  c(22L, 222L))
    expect_identical(loc2$line2, c(10L, 3333L))
    expect_identical(loc2$col2,  c(1L, 4L))
})

test_that("location() validates path", {
    expect_error(location(1L))
    expect_snapshot(location(1L), error = TRUE)
})

test_that("location() validates line1", {
    expect_error(location(line1 = ""))
    expect_error(location(line1 = 0L))
    expect_snapshot(location(line1 = ""), error = TRUE)
    expect_snapshot(location(line1 = 0L), error = TRUE)
})

test_that("location() validates col1", {
    expect_error(location(col1 = ""))
    expect_error(location(col1 = 0L))
    expect_snapshot(location(col1 = ""), error = TRUE)
    expect_snapshot(location(col1 = 0L), error = TRUE)
})

test_that("location() validates line2", {
    expect_error(location(line2 = ""))
    expect_error(location(line2 = 0L))
    expect_snapshot(location(line2 = ""), error = TRUE)
    expect_snapshot(location(line2 = 0L), error = TRUE)
})

test_that("location() validates col2", {
    expect_error(location(col2 = ""))
    expect_error(location(col2 = 0L))
    expect_snapshot(location(col2 = ""), error = TRUE)
    expect_snapshot(location(col2 = 0L), error = TRUE)
})

test_that("location() validates line1, col1, line2, and col2 have the same length", {
    expect_error(location(line1 = c(1L, 2L)))
    expect_snapshot(location(line1 = c(1L, 2L)), error = TRUE)
})

test_that("location() drops duplicated ranges", {
    loc <- location(path, c(1L, 1L), c(2L, 2L), c(3L, 3L), c(4L, 4L))
    expect_identical(loc$line1, 1L)
    expect_identical(loc$col1,  2L)
    expect_identical(loc$line2, 3L)
    expect_identical(loc$col2,  4L)
})

test_that("location() orders ranges", {
    loc <- location(path, c(2L, 1L), c(4L, 3L), c(6L, 5L), c(8L, 7L))
    expect_identical(loc$line1, c(1L, 2L))
    expect_identical(loc$col1,  c(3L, 4L))
    expect_identical(loc$line2, c(5L, 6L))
    expect_identical(loc$col2,  c(7L, 8L))
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
    # values is simpler.
    fmt_loc1 <- format(loc1)
    fmt_loc2 <- format(loc2)

    expect_type(fmt_loc1, "character")
    expect_type(fmt_loc2, "character")
    expect_length(fmt_loc1, 3L)
    expect_length(fmt_loc2, 5L)
    expect_identical(fmt_loc1, c(
        "<Location>",
        " Path: tests/testthat/my-test-file",
        " Ranges: Ln 1, Col 2 @ Ln 3, Col 4"))
    expect_identical(fmt_loc2, c(
        "<Location>",
        " Path: tests/testthat/my-test-file",
        " Ranges:",
        "  Ln  1, Col  22 @ Ln   10, Col 1",
        "  Ln 11, Col 222 @ Ln 3333, Col 4"))
})

# print.Location() -------------------------------------------------------------

test_that("print() works", {
    expect_output(print(loc1))
    expect_snapshot(print(loc1, "short"))
    expect_snapshot(print(loc2, "long"))
})

test_that("print() returns x invisibly", {
    withr::local_output_sink(tempfile())
    expect_invisible(print(loc1))
    expect_identical(print(loc1), loc1)
})

# c.Location -------------------------------------------------------------------

test_that("c.Location() returns a Location object", {
    out <- c(loc1, loc2)

    expect_s3_class(out, "Location")
    expect_identical(out$path,  loc1$path)
    expect_identical(out$line1, c(1L, 1L, 11L))
    expect_identical(out$col1,  c(2L, 22L, 222L))
    expect_identical(out$line2, c(3L, 10L, 3333L))
    expect_identical(out$col2,  c(4L, 1L, 4L))
})

test_that("c.Location() returns its single argument", {
    expect_identical(c(loc1), loc1)
})

test_that("c.Location() validates ...", {
    # The first argument passed to c() must be a
    # Location object. Otherwise, S3 dispatching
    # won't work as expected.
    expect_error(c(loc1, 1L, loc2))
    expect_snapshot(c(loc1, 1L, loc2), error = TRUE)
})

test_that("c.Location() throws an error if paths are not equal", {
    expect_error(c(location("a"), location("b")))
    expect_snapshot(c(location("a"), location("b")), error = TRUE)
})

# merge_locations() ------------------------------------------------------------

test_that("merge_locations() returns a list of Location object", {
    out <- merge_locations(location("a"), location("b"))
    expect_type(out, "list")
    expect_length(out, 2L)
})

test_that("merge_locations() validates ...", {
    expect_error(merge_locations(loc1, 1L, loc2))
    expect_snapshot(merge_locations(loc1, 1L, loc2), error = TRUE)
})

test_that("merge_locations() combines Location objects having different paths", {
    loc1 <- location("a", 1L, 1L, 1L, 1L)
    loc2 <- location("a", 2L, 2L, 2L, 2L)
    loc3 <- location("b", 3L, 3L, 3L, 3L)
    loc4 <- location("b", 4L, 4L, 4L, 4L)
    loc5 <- location("c", 5L, 5L, 5L, 5L)
    out  <- merge_locations(loc1, loc2, loc3, loc4, loc5)

    expect_length(out, 3L)
    expect_identical(out[[1L]], location("a", c(1L, 2L), c(1L, 2L), c(1L, 2L), c(1L, 2L)))
    expect_identical(out[[2L]], location("b", c(3L, 4L), c(3L, 4L), c(3L, 4L), c(3L, 4L)))
    expect_identical(out[[3L]], location("c", 5L, 5L, 5L, 5L))
})

# range_format() ---------------------------------------------------------------

test_that("range_format() returns a character", {
    out1 <- range_format(loc1)
    out2 <- range_format(loc2)

    expect_type(out1, "character")
    expect_type(out2, "character")
    expect_length(out1, 1L)
    expect_length(out2, 2L)
})

test_that("range_format() validates x", {
    expect_error(range_format(1L))
    expect_snapshot(range_format(1L), error = TRUE)
})

test_that("range_format() creates ranges according to the expected format", {
    expect_identical(range_format(loc1), "Ln 1, Col 2 @ Ln 3, Col 4")
    expect_identical(range_format(loc2), c(
        "Ln  1, Col  22 @ Ln   10, Col 1",
        "Ln 11, Col 222 @ Ln 3333, Col 4"))
})

# range_parse() ----------------------------------------------------------------

test_that("range_parse() returns a list of integers", {
    out <- range_parse(ranges)

    expect_type(out, "list")
    expect_length(out, 2L)
    expect_true(all(vapply_1l(out, is.integer)))
})

test_that("range_parse() validates strings", {
    expect_error(range_parse(1L))
    expect_snapshot(range_parse(1L), error = TRUE)
})

test_that("range_parse() suppresses warnings", {
    expect_no_warning(range_parse("Line 1, Column 2 @ Line 3, Column 4"))
})

test_that("range_parse() parses strings appropriately", {
    out <- range_parse(ranges)

    expect_identical(out[[1L]], c(1L, 22L, 10L, 1L))
    expect_identical(out[[2L]], c(11L, 222L, 3333L, 4L))

    expect_identical(
        range_parse("Ln 1, Col 2 @ Ln 3, Col 4")[[1L]],
        c(1L, 2L, 3L, 4L))
    expect_identical(
        range_parse("Ln  1, Col   2 @ Ln    3, Col    4")[[1L]],
        c(1L, 2L, 3L, 4L))
    expect_identical(
        range_parse("Ln 1.0, Col 2.34 @ Ln 3.14, Col 4.3")[[1L]],
        c(1L, 2L, 3L, 4L))

    expect_identical(
        range_parse("Line 1, Column 2 @ Line 3, Column 4")[[1L]],
        rep.int(NA_integer_, 4L))
    expect_identical(
        range_parse("Ln 1, Col 2 @ Ln 3, Col")[[1L]],
        rep.int(NA_integer_, 4L))
    expect_identical(
        range_parse("Ln X, Col 2 @ Ln 3, Col 4")[[1L]],
        rep.int(NA_integer_, 4L))
    expect_identical(
        range_parse("Ln X, Col 2 @@ Ln 3, Col 4")[[1L]],
        rep.int(NA_integer_, 4L))
})

# range_is_parseable() ---------------------------------------------------------

test_that("range_is_parseable() returns an logical", {
    out <- range_is_parseable(ranges)

    expect_type(out, "logical")
    expect_length(out, 2L)
})

test_that("range_is_parseable() validates strings", {
    expect_error(range_is_parseable(1L))
    expect_snapshot(range_is_parseable(1L), error = TRUE)
})

test_that("range_is_parseable() checks that strings can be parsed", {
    expect_true(range_is_parseable("Ln 1, Col 2 @ Ln 3, Col 4"))
    expect_true(range_is_parseable("Ln  1, Col   2 @ Ln    3, Col    4"))
    expect_true(range_is_parseable("Ln 1.0, Col 2.34 @ Ln 3.14, Col 4.3"))
    expect_false(range_is_parseable("Line 1, Column 2 @ Line 3, Column 4"))
    expect_false(range_is_parseable("Ln 1, Col 2 @ Ln 3, Col"))
    expect_false(range_is_parseable("Ln X, Col 2 @ Ln 3, Col 4"))
    expect_false(range_is_parseable("Ln X, Col 2 @@ Ln 3, Col 4"))
})
