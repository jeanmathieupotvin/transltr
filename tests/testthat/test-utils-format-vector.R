# TODO: to be revamped following new implementation.
skip("to be reviewed")

v  <- c(1L, 2L, 3L)

test_that("it returns a character vector", {
    out <- format_vector(v)
    expect_type(out, "character")
    expect_length(out, 3L)
})

test_that("it includes argument .top_level", {
    out <- format_vector(v, "Variable v:")
    expect_identical(out[[1L]], "Variable v:")
})

test_that("it trims output to 80 characters", {
    out <- format_vector(c(strrep("a", 100L), strrep("b", 100L)))
    expect_match(out, "\\.\\.\\.$")
    expect_identical(nchar(out[[1L]]), 80L)
    expect_identical(nchar(out[[2L]]), 80L)
})

test_that("it shows <nokey> if names are null or empty", {
    out <- format_vector(v)
    expect_match(out, "<nokey>: ")
})

test_that("it does not show <nokey> if show_nokey is false", {
    out <- format_vector(v, show_nokey = FALSE)
    expect_no_match(out, "<nokey>: ")
})

test_that("it indents values", {
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

test_that("it formats embedded structures accordingly", {
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
        cat(format_vector(struct, "<JohnDoe>", show_nokey = FALSE), sep = "\n")
    })
})
