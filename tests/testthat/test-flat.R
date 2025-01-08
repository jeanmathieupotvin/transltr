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
