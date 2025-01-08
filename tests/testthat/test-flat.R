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
