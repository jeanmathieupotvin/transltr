# flat_tag() -------------------------------------------------------------------


test_that("_tag() returns a character", {
    expect_type(flat_tag(), "character")
    expect_length(flat_tag(), 0L)

    expect_type(flat_tag(list(1L)), "character")
    expect_length(flat_tag(list(1L)), 1L)
})

test_that("_tag() validates x", {
    expect_error(flat_tag(1L))
    expect_snapshot(flat_tag(1L), error = TRUE)
})

test_that("_tag() validates tag_sep", {
    expect_error(flat_tag(tag_sep = 1L))
    expect_snapshot(flat_tag(tag_sep = 1L), error = TRUE)
})

test_that("_tag() validates tag_empty", {
    expect_error(flat_tag(tag_empty = 1L))
    expect_snapshot(flat_tag(tag_empty = 1L), error = TRUE)
})

test_that("_tag() creates tags from names", {
    x <- list(a = 1L, b = 2L, cd = list(c = 3L, d = 4L), e = list())

    expect_identical(flat_tag(x), c("a", "b", "cd: c", "cd: d", "e: "))
})

test_that("_tag() replaces empty names by positions and tag_empty", {
    x <- list(a = 1L, b = 2L, list())

    expect_identical(flat_tag(x), c("a", "b", "[3]: "))
    expect_identical(flat_tag(x, tag_empty = "_"), c("a", "b", "_[3]: "))
})

test_that("_tag() retains the structure of x when replacing empty names by positions", {
    x <- list(1L, 2L, list(3L, 4L), 5L)

    expect_identical(flat_tag(x), c("[1]", "[2]", "[3]: [1]", "[3]: [2]", "[4]"))
})


# flat_format() ----------------------------------------------------------------


test_that("_format() returns a list having the same shape as x", {
    # Lengths, types, and attributes (like names) must be preserved.
    x <- list(a = 1L, b = 2L, cd = list(c = 3L, d = 4L))

    expect_identical(
        flat_format(x),
        list(a = "1", b = "2", cd = list(c = "3", d = "4")))
})

test_that("_format() coerces elements of x to character strings", {
    # We trust format() works as expected and only check
    # whether paste0() is called appropriately. We also
    # check this is done recursively.
    x <- list(
        a = logical(2L),
        b = integer(2L),
        c = double(2L),
        d = complex(2L),
        e = character(2L),
        f = raw(2L),
        g = list(
            a = logical(2L),
            b = integer(2L),
            c = double(2L),
            d = complex(2L),
            e = c("a", "b"),
            f = raw(2L)))

    expect_identical(
        flat_format(x),
        list(
            a = "FALSE\nFALSE",
            b = "0\n0",
            c = "0\n0",
            d = "0+0i\n0+0i",
            e = "\n",
            f = "00\n00",
            g = list(
                a = "FALSE\nFALSE",
                b = "0\n0",
                c = "0\n0",
                d = "0+0i\n0+0i",
                e = "a\nb",
                f = "00\n00")))
})

test_that("_format() replaces empty contents of empty lists by a constant", {
    expect_identical(flat_format(list()), list("<empty list>"))
    expect_identical(flat_format(list(list())), list(list("<empty list>")))
    expect_identical(flat_format(list(list(list()))), list(list(list("<empty list>"))))
})

test_that("_format() validates x", {
    expect_error(flat_format(1L))
    expect_snapshot(flat_format(1L), error = TRUE)
})


# flat_example() ---------------------------------------------------------------


test_that("_example() returns a character string invisibly", {
    out <- flat_example()

    expect_type(out, "character")
    expect_length(out, 1L)
    expect_invisible(flat_example())
})

test_that("_example() prints the example before returning", {
    expect_output(flat_example())
    expect_snapshot(flat_example())
})
