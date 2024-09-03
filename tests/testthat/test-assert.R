# assert_arg() can only be called within another function.
# We wrap it with wrap_assert_arg() to test it.
wrap_assert_arg <- function(
    my_x = c(1L, 2L),
    quote_values = FALSE,
    throw_error  = TRUE)
{
    return(assert_arg(my_x, quote_values, throw_error))
}

# wrap_assert_arg_alt() is a variant of wrap_assert_arg()
# that returns my_x instead. It is useful to test whether
# assert_arg() assigns default value of my_x within the
# parent environment (of the function that called it).
wrap_assert_arg_alt <- function(my_x = c(1L, 2L)) {
    assert_arg(my_x)
    return(my_x)
}


# is_chr() ---------------------------------------------------------------------


test_that("is_chr() returns a logical", {
    expect_true(is_chr(""))
    expect_false(is_chr(1L))
})

test_that("is_chr() disallows empty vectors by default", {
    expect_false(is_chr(character()))
    expect_true(is_chr(character(), allow_empty = TRUE))
})

test_that("is_chr() disallows NAs", {
    expect_false(is_chr(NA_character_))
})


# is_int1() --------------------------------------------------------------------


test_that("is_int1() returns a logical", {
    expect_true(is_int1(1L))
    expect_false(is_int1("1"))
})

test_that("is_int1() disallows any length not equal to 1", {
    expect_false(is_int1(integer()))
    expect_false(is_int1(integer(2L)))
})

test_that("is_int1() disallows NAs", {
    expect_false(is_int1(NA))
})


# is_chr1() --------------------------------------------------------------------


test_that("is_chr1() returns a logical", {
    expect_true(is_chr1("1"))
    expect_false(is_chr1(1L))
})

test_that("is_chr1() disallows any length not equal to 1", {
    expect_false(is_chr1(character()))
    expect_false(is_chr1(character(2L)))
})

test_that("is_chr1() disallows empty values by default", {
    expect_false(is_chr1(""))
    expect_true(is_chr1("", allow_empty_string = TRUE))
})

test_that("is_chr1() disallows NAs", {
    expect_false(is_chr1(NA_character_))
})


# is_list() --------------------------------------------------------------------


test_that("is_list() returns a logical", {
    expect_true(is_list(list(1L)))
    expect_false(is_list(1L))
})

test_that("is_list() disallows empty lists by default", {
    expect_false(is_list(list()))
    expect_true(is_list(list(), allow_empty = TRUE))
})


# is_between() ----------------------------------------------------------------


test_that("is_between() returns a logical", {
    expect_true(is_between(1.0))
    expect_true(is_between(1L))
    expect_false(is_between("1"))
})

test_that("is_between() disallows any length not equal to 1", {
    expect_false(is_between(integer()))
    expect_false(is_between(integer(2L)))
})

test_that("is_between() disallows NAs", {
    expect_false(is_between(NA_integer_))
    expect_false(is_between(NA_real_))
})

test_that("is_between() enforces lower bound (min)", {
    expect_true(is_between(1.0,  min = 1.0))
    expect_false(is_between(1.0, min = 1.1))
    expect_true(is_between(1L,   min = 1L))
    expect_false(is_between(1L,  min = 2L))
})

test_that("is_between() enforces upper bound (max)", {
    expect_true(is_between(1.0,  max = 1.0))
    expect_false(is_between(1.0, max = 0.9))
    expect_true(is_between(1L,   max = 1L))
    expect_false(is_between(1L,  max = 0L))
})


# is_named() -------------------------------------------------------------------


test_that("is_named() returns a logical", {
    expect_true(is_named(list(a = 1L)))
    expect_false(is_named(list(1L)))
})

test_that("is_named() returns true for empty vectors", {
    expect_true(is_named(list()))
})

test_that("is_named() disallows null names", {
    expect_false(is_named(list(1L)))
})

test_that("is_named() disallows empty names by default", {
    a_list <- list(a = 1L, 2L)

    expect_false(is_named(a_list))
    expect_true(is_named(a_list, allow_empty_names = TRUE))
})

test_that("is_named() disallows NA names by default", {
    a_list <- list(a = 1L, 2L)
    names(a_list) <- c("a", NA_character_)

    expect_false(is_named(a_list))
    expect_true(is_named(a_list, allow_na_names = TRUE))
})


# is_match() -------------------------------------------------------------------


test_that("is_match() returns a logical", {
    expect_true(is_match(1L,  c(1L, 2L)))
    expect_false(is_match(3L, c(1L, 2L)))
})

test_that("is_named() returns false for empty value (empty x)", {
    expect_false(is_match(integer(0L)))
})

test_that("is_named() returns false for empty choices", {
    expect_false(is_match(1L, integer(0L)))
})

test_that("is_match() exactly matches x by default", {
    expect_false(is_match("a", c("aa", "bb")))
    expect_true(is_match("a",  c("aa", "bb"), allow_partial = TRUE))
})


# assert_chr() -----------------------------------------------------------------


test_that("assert_chr() returns an empty character if x is valid", {
    expect_identical(assert_chr("a"), "")
})

test_that("assert_chr() throws an error by default if x is invalid", {
    expect_error(assert_chr(1L))
})

test_that("assert_chr() adapts its error message(s)", {
    expect_identical(
        assert_chr(1L, throw_error = FALSE),
        "'1L' must be a non-empty character vector of non-NA values.")
    expect_identical(
        assert_chr(1L, allow_empty = TRUE, throw_error = FALSE),
        "'1L' must be a character vector of non-NA values.")

    expect_snapshot(assert_chr(1L),                     error = TRUE)
    expect_snapshot(assert_chr(1L, allow_empty = TRUE), error = TRUE)
})

test_that("assert_chr() sets argument's name", {
    my_x <- 1L

    expect_match(assert_chr(my_x, throw_error = FALSE),               "^'my_x'")
    expect_match(assert_chr(my_x, throw_error = FALSE, x_name = "x"), "^'x'")
})


# assert_chr1() ----------------------------------------------------------------


test_that("assert_chr1() returns an empty character if x is valid", {
    expect_identical(assert_chr1("a"), "")
})

test_that("assert_chr1() throws an error by default if x is invalid", {
    expect_error(assert_chr1(1L))
})

test_that("assert_chr1() adapts its error message(s)", {
    expect_identical(
        assert_chr1(1L, throw_error = FALSE),
        "'1L' must be a non-NA and non-empty character of length 1.")
    expect_identical(
        assert_chr1(1L, allow_empty_string = TRUE, throw_error = FALSE),
        "'1L' must be a non-NA character of length 1.")

    expect_snapshot(assert_chr1(1L),                            error = TRUE)
    expect_snapshot(assert_chr1(1L, allow_empty_string = TRUE), error = TRUE)
})

test_that("assert_chr1() sets argument's name", {
    my_x <- 1L

    expect_match(assert_chr1(my_x, throw_error = FALSE),               "^'my_x'")
    expect_match(assert_chr1(my_x, throw_error = FALSE, x_name = "x"), "^'x'")
})


# assert_int1() ----------------------------------------------------------------


test_that("assert_int1() returns an empty character if x is valid", {
    expect_identical(assert_int1(1L), "")
})

test_that("assert_int1() throws an error by default if x is invalid", {
    expect_error(assert_int1(1.0))
})

test_that("assert_int1() adapts its error message(s)", {
    expect_identical(
        assert_int1(1.0, throw_error = FALSE),
        "'1' must be a non-NA integer of length 1.")

    # assert_int1() does not adapt its error message.
    # It is constant. But we snapshot it anyway with
    # a comment for consistency.
    expect_snapshot(error = TRUE, {
        "Error message of assert_int1() is constant."
        assert_int1(1.0)
    })
})

test_that("assert_int1() sets argument's name", {
    my_x <- 1.0

    expect_match(assert_int1(my_x, throw_error = FALSE),               "^'my_x'")
    expect_match(assert_int1(my_x, throw_error = FALSE, x_name = "x"), "^'x'")
})


# assert_list() ----------------------------------------------------------------


test_that("assert_list() returns an empty character if x is valid", {
    expect_identical(assert_list(list(1L)), "")
})

test_that("assert_list() throws an error by default if x is invalid", {
    expect_error(assert_list(list()))
})

test_that("assert_list() adapts its error message(s)", {
    expect_identical(
        assert_list(1L, throw_error = FALSE),
        "'1L' must be a non-empty list.")
    expect_identical(
        assert_list(1L, allow_empty = TRUE, throw_error = FALSE),
        "'1L' must be a list.")

    expect_snapshot(assert_list(1L),                     error = TRUE)
    expect_snapshot(assert_list(1L, allow_empty = TRUE), error = TRUE)
})

test_that("assert_list() sets argument's name", {
    my_x <- 1L

    expect_match(assert_list(my_x, throw_error = FALSE),               "^'my_x'")
    expect_match(assert_list(my_x, throw_error = FALSE, x_name = "x"), "^'x'")
})


# assert_between() ------------------------------------------------------------


test_that("assert_between() returns an empty character if x is valid", {
    expect_identical(assert_between(1L), "")
})

test_that("assert_between() throws an error by default if x is invalid", {
    expect_error(assert_between(1L, max = 0L))
})

test_that("assert_between() adapts its error message(s)", {
    expect_identical(
        assert_between(1i, throw_error = FALSE),
        "'0+1i' must be a non-NA numeric value.")
    expect_identical(
        assert_between(1L, max = 0L, throw_error = FALSE),
        "'1L' must be a non-NA numeric value in the range (-Inf, 0].")
    expect_identical(
        assert_between(1L, min = 2L, throw_error = FALSE),
        "'1L' must be a non-NA numeric value in the range [2, Inf).")
    expect_identical(
        assert_between(1L, min = 2L, max = 3L, throw_error = FALSE),
        "'1L' must be a non-NA numeric value in the range [2, 3].")

    expect_snapshot(assert_between(1i),                     error = TRUE)
    expect_snapshot(assert_between(1L, max = 0L),           error = TRUE)
    expect_snapshot(assert_between(1L, min = 2L),           error = TRUE)
    expect_snapshot(assert_between(1L, min = 2L, max = 3L), error = TRUE)
})

test_that("assert_between() sets argument's name", {
    my_x <- 1L

    expect_match(assert_between(my_x, min = 2L, throw_error = FALSE),               "^'my_x'")
    expect_match(assert_between(my_x, min = 2L, throw_error = FALSE, x_name = "x"), "^'x'")
})


# assert_names() ---------------------------------------------------------------


test_that("assert_names() returns an empty character if x is valid", {
    expect_identical(assert_names(list()), "")
    expect_identical(assert_names(list(a = 1L)), "")
})

test_that("assert_names() throws an error by default if x is invalid", {
    expect_error(assert_names(list(1L)))
})

test_that("assert_names() adapts its error message(s)", {
    expect_identical(
        assert_names(list(1L), throw_error = FALSE),
        "'list(1L)' must have names.")
    expect_identical(
        assert_names(list(1L), allow_empty_names = TRUE, throw_error = FALSE),
        "'list(1L)' must have names. They can be empty strings.")
    expect_identical(
        assert_names(list(1L), allow_na_names = TRUE, throw_error = FALSE),
        "'list(1L)' must have names. They can be NA values.")
    expect_identical(
        assert_names(list(1L),
            allow_empty_names = TRUE,
            allow_na_names    = TRUE,
            throw_error       = FALSE),
        "'list(1L)' must have names. They can be empty strings. They can be NA values.")

    expect_snapshot(assert_names(list(1L)),                           error = TRUE)
    expect_snapshot(assert_names(list(1L), allow_empty_names = TRUE), error = TRUE)
    expect_snapshot(assert_names(list(1L), allow_na_names    = TRUE), error = TRUE)
    expect_snapshot(error = TRUE,
        assert_names(list(1L),
            allow_empty_names = TRUE,
            allow_na_names    = TRUE))
})

test_that("assert_names() sets argument's name", {
    my_x <- 1L

    expect_match(assert_names(my_x, throw_error = FALSE),               "^'my_x'")
    expect_match(assert_names(my_x, throw_error = FALSE, x_name = "x"), "^'x'")
})


# assert_match() ---------------------------------------------------------------


test_that("assert_match() returns an empty character if x is valid", {
    expect_identical(assert_match(1L, c(1L, 2L)), "")
})

test_that("assert_match() throws an error by default if x is invalid", {
    expect_error(assert_match(3L, c(1L, 2L)))
})

test_that("assert_match() adapts its error message(s)", {
    expect_identical(
        assert_match(3L, c(1L, 2L), throw_error = FALSE),
        "'3L' must be equal to 1 or 2.")

    # assert_match() does not adapt its error message.
    # It is constant. But we snapshot it anyway with a
    # comment for consistency.
    expect_snapshot(error = TRUE, {
        "Error message of assert_match() is constant."
        assert_match(3L, c(1L, 2L))
    })
})

test_that("assert_match() does not quote values by default", {
    expect_identical(
        assert_match(3L, c(1L, 2L), throw_error = FALSE),
        "'3L' must be equal to 1 or 2.")
    expect_identical(
        assert_match(3L, c(1L, 2L), quote_values = TRUE, throw_error = FALSE),
        "'3L' must be equal to '1' or '2'.")

    expect_snapshot(assert_match(3L, c(1L, 2L)),                      error = TRUE)
    expect_snapshot(assert_match(3L, c(1L, 2L), quote_values = TRUE), error = TRUE)
})

test_that("assert_match() sets argument's name", {
    my_x <- 3L

    expect_match(assert_match(my_x, c(1L, 2L), throw_error = FALSE),               "^'my_x'")
    expect_match(assert_match(my_x, c(1L, 2L), throw_error = FALSE, x_name = "x"), "^'x'")
})


# assert_arg() -----------------------------------------------------------------


test_that("assert_arg() returns an empty character if x is valid", {
    expect_identical(wrap_assert_arg(1L), "")
})

test_that("assert_arg() throws an error by default if x is invalid", {
    expect_error(wrap_assert_arg(3L))
})

test_that("assert_arg() adapts its error message(s)", {
    expect_identical(
        wrap_assert_arg(3L, throw_error = FALSE),
        "'my_x' must be equal to 1 or 2.")

    # assert_arg() does not adapt its error message.
    # It is constant. But we snapshot it anyway with
    # a comment for consistency.
    expect_snapshot(error = TRUE, {
        "Error message of assert_arg() is constant."
        wrap_assert_arg(3L)
    })
})

test_that("assert_arg() does not quote values by default", {
    expect_identical(
        wrap_assert_arg(3L, throw_error = FALSE),
        "'my_x' must be equal to 1 or 2.")
    expect_identical(
        wrap_assert_arg(3L, quote_values = TRUE, throw_error = FALSE),
        "'my_x' must be equal to '1' or '2'.")

    expect_snapshot(wrap_assert_arg(3L),                      error = TRUE)
    expect_snapshot(wrap_assert_arg(3L, quote_values = TRUE), error = TRUE)
})

test_that("assert_arg() sets argument's name", {
    expect_match(wrap_assert_arg(3L, throw_error = FALSE), "^'my_x'")
})

test_that("assert_arg() assigns default value in parent env if x is missing", {
    expect_identical(wrap_assert_arg_alt(), 1L)
})
