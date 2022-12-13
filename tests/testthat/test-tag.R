# Fixtures ---------------------------------------------------------------------


newTestStrings <- function() {
    strings <- c(
        "",
        " ",
        "{{Tagged string.}}",
        "{{ Tagged string.\n With a special character. }}",
        "{{  Tagged string with inner spaces.  }}",
        "  {{    Tagged string with outer spaces.    }}  ",
        "\" {{ Tagged string with double quotes. }} \"")

    return(strings)
}


# Tests ------------------------------------------------------------------------


test_that("tag() and untag() works properly", {
    strings <- newTestStrings()

    expect_snapshot_output({
        cat("Strings to be tagged and untagged.\n\n")
        strings
    })
    expect_snapshot_output({
        cat("Strings normalized and tagged.\n\n")
        tag(strings)
    })
    expect_snapshot_output({
        cat("Strings untagged. Double quotes are kept.\n\n")
        untag(strings, TRUE)
    })
    expect_snapshot_output({
        cat("Strings untagged. Double quotes are dropped.\n\n")
        untag(strings, FALSE)
    })
})

test_that("tag() wraps strings", {
    expect_identical(tag(c("a", "b")), c("{{ a }}", "{{ b }}"))
})

test_that("tag() returns an empty vector if strings is empty", {
    expect_identical(tag(), character())
})

test_that("tag() returns a character vector", {
    s <- newTestStrings()

    expect_type(tag(s), "character")
    expect_length(tag(s), length(s))
})

test_that("tag() validates strings", {
    expect_error(tag(1L))
    expect_error(tag(NA_character_))
    expect_snapshot_error(tag(1L))
})

test_that("tag() validates normalize", {
    expect_error(tag("test", 1L))
    expect_error(tag("test", c(TRUE, TRUE)))
    expect_error(tag("test", NA))
    expect_snapshot_error(tag("test", 1L))
})

test_that("tag() omits normalization if normalize is false", {
    expect_identical(tag("{{ a }}", FALSE), "{{ {{ a }} }}")
})

test_that("tag() omits empty strings", {
    expect_identical(tag(""), "")
    expect_identical(tag(c("", "a")), c("", "{{ a }}"))
})

test_that("untag() unwraps strings", {
    s <- newTestStrings()
    expected <- c(
        "",
        " ",
        "Tagged string.",
        "Tagged string.\n With a special character.",
        "Tagged string with inner spaces.",
        "Tagged string with outer spaces.",
        "Tagged string with double quotes.")

    expect_identical(untag(s), expected)
})

test_that("untag() returns an empty vector if strings is empty", {
    expect_identical(untag(), character())
})

test_that("untag() validates strings", {
    expect_error(untag(1L))
    expect_error(untag(NA_character_))
    expect_snapshot_error(untag(1L))
})

test_that("untag() validates keepDoubleQuotes", {
    expect_error(untag("{{ test }}", 1L))
    expect_error(untag("{{ test }}", c(TRUE, TRUE)))
    expect_error(untag("{{ test }}", NA))
    expect_snapshot_error(untag("{{ test }}", 1L))
})

test_that("untag() keeps double quotes if keepDoubleQuotes is true", {
    string   <- "\" {{ Tagged string with double quotes. }} \""
    expected <- "\"Tagged string with double quotes.\""

    expect_identical(untag(string, TRUE), expected)
})

test_that("isTagged() returns a logical vector", {
    s <- newTestStrings()

    expect_identical(isTagged(s), c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE))
})

test_that("isTagged() validates strings", {
    expect_false(isTagged(1L))
    expect_false(isTagged())
})
