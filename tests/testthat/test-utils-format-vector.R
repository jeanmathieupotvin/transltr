x1 <- list(
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

x2 <- list(
    level1 = 0L,
    level2 = list(
        a = 1L,
        b = 2L,
        3L,
    level3 = list(
        c = 4L,
        d = 5L,
        e = list())),
    level1 = c(
        f = 6L,
        g = 7L),
    level1 = list(
        h = NULL,
        i = 8L,
        j = 9L))

test_that("it returns a character vector", {
    out_empty <- format_vector()
    out_x1    <- format_vector(x1)
    out_x2    <- format_vector(x2)

    expect_type(out_empty, "character")
    expect_type(out_x1, "character")
    expect_type(out_x2, "character")

    # x1:  8 values + 2 labels for nested vectors with more than 1 elements.
    # x2: 12 values + 4 labels for nested vectors with more than 1 elements.
    expect_length(out_empty, 0L)
    expect_length(out_x1, 10L)
    expect_length(out_x2, 16L)

    expect_snapshot(cat(format_vector(x1, "<Test: x1>"), sep = "\n"))
    expect_snapshot({
        cat(sep = "\n", format_vector(
            x2,
            label  = "<Test: x2>",
            indent = 2L,
            fill_names = TRUE))
    })
})

test_that("it trims outputs to 80 characters", {
    long_values <- list(
        a = strrep("a", 200L),
        b = strrep("b", 200L))

    expect_identical(nchar(format_vector(long_values)), c(80L, 80L))
})

test_that("it validates validate", {
    expect_error(format_vector(validate = 1L))
    expect_snapshot(format_vector(validate = 1L), error = TRUE)
})

test_that("it validates label if validate is true", {
    expect_no_error(format_vector(label = NULL))

    # Disallowed by design, but we can still get an
    # output in practice if label is an atomic value
    # and validate is false (label is used as is).
    expect_no_error(format_vector(label = 1L, validate = FALSE))

    expect_error(format_vector(label = 1L))
    expect_snapshot(format_vector(label = 1L), error = TRUE)
})

test_that("it validates level if validate is true", {
    # Disallowed by design, but we can still get
    # an output in practice if label is negative.
    expect_no_error(format_vector(level = -1L, validate = FALSE))

    expect_error(format_vector(level = "1"))
    expect_error(format_vector(level = -1L))
    expect_snapshot(format_vector(level = "1"), error = TRUE)
    expect_snapshot(format_vector(level = -1L), error = TRUE)
})

test_that("it validates indent if validate is true", {
    # Disallowed by design, but we can still get
    # an output in practice if indent is negative.
    expect_no_error(format_vector(indent = -1L, validate = FALSE))

    expect_error(format_vector(indent = " "))
    expect_error(format_vector(indent = -1L))
    expect_snapshot(format_vector(indent = " "), error = TRUE)
    expect_snapshot(format_vector(indent = -1L), error = TRUE)
})

test_that("it validates fill_names if validate is true", {
    # Disallowed by design, but we can still get
    # an output in practice if fill_names is an
    # integer. if() coerces it to a logical.
    expect_no_error(format_vector(fill_names = 1L, validate = FALSE))

    expect_error(format_vector(fill_names = 1L))
    expect_snapshot(format_vector(fill_names = 1L), error = TRUE)
})

test_that("it validates null if validate is true", {
    # Disallowed by design, but we can still get
    # an output in practice if null is not a character.
    expect_no_error(format_vector(null = -999L, validate = FALSE))

    expect_error(format_vector(null = 1L))
    expect_snapshot(format_vector(null = 1L), error = TRUE)
})

test_that("it validates empty if validate is true", {
    # Disallowed by design, but we can still get
    # an output in practice if empty is not a
    # character.
    expect_no_error(format_vector(empty = pairlist(), validate = FALSE))

    expect_error(format_vector(empty = 1L))
    expect_snapshot(format_vector(empty = 1L), error = TRUE)
})

test_that("it includes label when it is not null", {
    # The very first label should not be indented either (by default).
    expect_identical(format_vector(x1, "<Person>")[[1L]], "<Person>:")

    # If format_vector() starts at level 2, then it should be indented
    # once (level - 1) and so on.
    expect_identical(format_vector(x1, "<Person>", level = 2L)[[1L]], " <Person>:")
    expect_identical(format_vector(x1, "<Person>", level = 3L)[[1L]], "  <Person>:")
    expect_identical(format_vector(x1, "<Person>", level = 4L)[[1L]], "   <Person>:")
})

test_that("it replaces empty names if fill_names is true", {
    names_null    <- list(1L, 2L, list(3L, 4L))
    names_missing <- list(a = 1L, 2L, list(3L, d = 4L))

    out_null    <- format_vector(names_null, fill_names = TRUE)
    out_missing <- format_vector(names_missing, fill_names = TRUE)

    # All elements should get keys corresponding to their position
    # (relative to each level).
    expect_identical(format_vector(names_null, fill_names = TRUE), c(
        "[1]: 1",
        "[2]: 2",
        "[3]:",
        " [1]: 3",
        " [2]: 4"))

    # Missing keys should be replaced by keys corresponding to
    # their position (relative to each level).
    expect_identical(format_vector(names_missing, fill_names = TRUE), c(
        "a: 1",
        "[2]: 2",
        "[3]:",
        " [1]: 3",
        " d: 4"))
})

test_that("it replaces (true) null objects by null", {
    # Test that lower-level values are also replaced.
    out <- format_vector(list(a = NULL, list(b = NULL)))

    expect_identical(out[[1L]], "a: <null>")
    expect_identical(out[[3L]], " b: <null>")
})

test_that("it replaces empty objects by empty", {
    # Beware! Empty parlists are conceptually the
    # same thing as NULL. This is documented in
    # the Details of ?list.
    out <- format_vector(list(
        a = logical(0L),
        b = integer(0L),
        c = double(0L),
        d = complex(0L),
        e = character(0L),
        f = raw(0L),
        g = list(),
        h = pairlist(),
        # Test that lower-level values are also replaced.
        i = list(
            j = logical(0L),
            k = integer(0L),
            l = double(0L),
            m = complex(0L),
            n = character(0L),
            o = raw(0L),
            p = list(),
            q = pairlist())))

    expect_identical(out[[1L]],  "a: <empty> [logical]")
    expect_identical(out[[2L]],  "b: <empty> [integer]")
    expect_identical(out[[3L]],  "c: <empty> [double]")
    expect_identical(out[[4L]],  "d: <empty> [complex]")
    expect_identical(out[[5L]],  "e: <empty> [character]")
    expect_identical(out[[6L]],  "f: <empty> [raw]")
    expect_identical(out[[7L]],  "g: <empty> [list]")
    expect_identical(out[[8L]],  "h: <null>")
    expect_identical(out[[10L]], " j: <empty> [logical]")
    expect_identical(out[[11L]], " k: <empty> [integer]")
    expect_identical(out[[12L]], " l: <empty> [double]")
    expect_identical(out[[13L]], " m: <empty> [complex]")
    expect_identical(out[[14L]], " n: <empty> [character]")
    expect_identical(out[[15L]], " o: <empty> [raw]")
    expect_identical(out[[16L]], " p: <empty> [list]")
    expect_identical(out[[17L]], " q: <null>")
    expect_snapshot({
        "An example of format_vector() replacing empty objects."
        "See the underlying test block to inspect the input."
        cat(out, sep = "\n")
    })
})

test_that("it replaces empty character strings by pairs of double quotes", {
    out <- format_vector(list(a = "a", b = "", c = "c"))

    # It works as a single element.
    expect_identical(format_vector(list("")), r"{""}")

    # It works as a child element of a list.
    expect_identical(out, c("a: a", r"{b: ""}", "c: c"))

    # It works as a value of a character.
    expect_identical(
        format_vector(list(v = c(a = "a", b = "", c = "c"))),
        c("v:", " a: a", r"{ b: ""}", " c: c"))

    expect_snapshot({
        "An example of format_vector() replacing empty character strings."
        "See the underlying test block to inspect the input."
        cat(out, sep = "\n")
    })
})
