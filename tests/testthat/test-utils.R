test_that(".stopf() throws errors", {
    expect_error(.stopf("error"))
})

test_that(".stopf() concatenates error messages", {
    expect_snapshot_error(.stopf(c("message 1 and ", "message 2")))
})

test_that(".stopf() substitutes placeholders with sprintf()", {
    expect_snapshot_error(.stopf("the %s was substituted", "placeholder"))
})

test_that(".map() returns a list", {
    out <- .map(`+`, c(1L, 2L), c(3L, 4L))

    expect_type(out, "list")
    expect_length(out, 2L)
    expect_identical(out, list(4L, 6L))
})

test_that(".vapply1i() returns an integer", {
    out <- .vapply1i(c(one = 1L, two = 2L), `+`, e2 = 1L)

    expect_type(out, "integer")
    expect_length(out, 2L)
    expect_null(names(out))
    expect_identical(out, c(2L, 3L))
})

test_that(".vapply1c() returns a character", {
    out <- .vapply1c(c(one = "1", two = "2"), paste0, "1")

    expect_type(out, "character")
    expect_length(out, 2L)
    expect_null(names(out))
    expect_identical(out, c("11", "21"))
})
