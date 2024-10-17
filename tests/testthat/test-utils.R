v <- c(1L, 2L, 3L)


# vapply_1l() ------------------------------------------------------------------


test_that("vapply_1l() returns a logical vector", {
    out <- vapply_1l(v, `>`, e2 = 0L)

    expect_type(out, "logical")
    expect_length(out, length(v))
    expect_named(v, NULL)
})


# vapply_1i() ------------------------------------------------------------------


test_that("vapply_1i() returns a logical vector", {
    out <- vapply_1i(v, `+`, e2 = 0L)

    expect_type(out, "integer")
    expect_length(out, length(v))
    expect_named(v, NULL)
})


# vapply_1c() ------------------------------------------------------------------


test_that("vapply_1c() returns a character vector", {
    out <- vapply_1c(v, as.character)

    expect_type(out, "character")
    expect_length(out, length(v))
    expect_named(v, NULL)
})


# stops() ----------------------------------------------------------------------


test_that("stops() does not return the call as part of the error message", {
    expect_error(stops())
    expect_snapshot(error = TRUE, {
        wrap_stops <- \() stops("this is an error message.")
        wrap_stops()
    })
})


# stopf() ----------------------------------------------------------------------


test_that("stopf() works", {
    expect_error(stopf())
    expect_snapshot(
        stopf("this '%s' becomes part of the error message.", "placeholder"),
        error = TRUE)
})


# split_ul() -------------------------------------------------------------------


test_that("split_ul() works", {
    x   <- c(1L, 2L, 2L, 3L, 3L, 3L)
    out <- split_ul(x, x)

    expect_identical(out, list(1L, c(2L, 2L), c(3L, 3L, 3L)))
    expect_named(out, NULL)
})


# format_vector() --------------------------------------------------------------


test_that("format_vector() returns a character vector", {
    out <- format_vector(v)
    expect_type(out, "character")
    expect_length(out, 3L)
})

test_that("format_vector() includes argument .top_level", {
    out <- format_vector(v, "Variable v:")
    expect_identical(out[[1L]], "Variable v:")
})

test_that("format_vector() trims output to 80 characters", {
    out <- format_vector(c(strrep("a", 100L), strrep("b", 100L)))
    expect_match(out, "\\.\\.\\.$")
    expect_identical(nchar(out[[1L]]), 80L)
    expect_identical(nchar(out[[2L]]), 80L)
})

test_that("format_vector() shows <nokey> if names are null or empty", {
    out <- format_vector(v)
    expect_match(out, "<nokey>: ")
})

test_that("format_vector() does not show <nokey> if .show_nokey is false", {
    out <- format_vector(v, .show_nokey = FALSE)
    expect_no_match(out, "<nokey>: ")
})

test_that("format_vector() indents values", {
    out <- format_vector(
        list(
            a = 1L,
            b = list(
                c = 2L,
                d = list(
                    e = 3L,
                    f = 4L))))

    expect_match(out[[1L]], "^  ")      # 2 spaces (level 1)
    expect_match(out[[2L]], "^  ")      # 2 spaces (level 1)
    expect_match(out[[3L]], "^    ")    # 4 spaces (level 2)
    expect_match(out[[4L]], "^    ")    # 4 spaces (level 2)
    expect_match(out[[5L]], "^      ")  # 6 spaces (level 3)
    expect_match(out[[6L]], "^      ")  # 6 spaces (level 3)
    expect_snapshot(cat(out, sep = "\n"))
})

test_that("format_vector() formats embedded structures accordingly", {
    # This test block generalizes previous one
    # above to expected uses cases of the package.
    struct <- list(
        FirstName = "John",
        LastName  = "Doe",
        Address   = list(
            StreetAddress = "123 Main Street",
            City          = "Montreal",
            Province      = "Quebec",
            PostalCode    = "H0H 0H0"),
        Notes = c(
            "Send mail to",
            "address above."))

    expect_snapshot(cat(format_vector(struct, "<JohnDoe>"), sep = "\n"))
    expect_snapshot({
        cat(format_vector(struct, "<JohnDoe>", .show_nokey = FALSE), sep = "\n")
    })
})


# `%??%` -----------------------------------------------------------------------


test_that("operator %??% works", {
    expect_null(NULL %??% NULL)
    expect_identical(NULL %??% 1L, 1L)
    expect_identical(1L %??% NULL, 1L)
})
