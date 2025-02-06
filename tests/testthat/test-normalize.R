test_that("it returns a character string", {
    out <- normalize("a", "b", "c")

    expect_type(out, "character")
    expect_length(out, 1L)
})

test_that("it concatenates values passed to ...", {
    expect_identical(normalize("a", "b", "c"),     "abc")
    expect_identical(normalize("a", c("b", "c")),  "abc")
})

test_that("it replaces whitespaces by a single space", {
    str <- c(
        "Many spaces\nand    \t tabs. ",
        "Many \t\t  tabs and spaces. ",
        "Many tabs and\n\t newlines.")

    expect_identical(
        normalize(str),
        "Many spaces and tabs. Many tabs and spaces. Many tabs and newlines.")
})

test_that("it discards leading and trailing empty strings", {
    str <- c(
        "",
        "",
        "Contents is here.",
        "Continued.",
        "",
        "")

    expect_identical(normalize(str), "Contents is here.Continued.")
})

test_that("it inserts paragraph separators into empty non-leading/trailing strings", {
    str <- c(
        "",
        "",
        "Contents is here.",
        "", # This is a paragraph separator.
        "Continued.",
        "",
        "")

    expect_identical(normalize(str), "Contents is here.\n\nContinued.")
})

test_that("it removes leading and trailing whitespaces", {
    str <- c(
        "\n\t  Leading whitespaces. ",
        "Trailing whitespaces.\n\t  ")

    expect_identical(
        normalize(str),
         "Leading whitespaces. Trailing whitespaces.")
})

test_that("it works as expected on plausible real cases", {
    x1 <- "
        Lorem Ipsum is simply dummy text of the printing and typesetting industry.

        Lorem Ipsum has been the industry's standard dummy text ever since the
        1500s, when an unknown printer    took a galley of type and scrambled it
        to make a type specimen book."

    x2 <- "
        Lorem Ipsum is simply dummy text of the printing and typesetting industry.

            Lorem Ipsum has been the industry's standard dummy text ever
            since the 1500s, when an unknown printer took a galley of type
            and scrambled it to make a type specimen book."

    expected <- paste(sep = " ",
        "Lorem Ipsum is simply dummy text of the printing and typesetting",
        "industry. Lorem Ipsum has been the industry's standard dummy text",
        "ever since the 1500s, when an unknown printer took a galley of",
        "type and scrambled it to make a type specimen book.")

    expect_identical(
        normalize("
            This is a multi-line string.
            It can be convenient to writre litteral values like this."),
        "This is a multi-line string. It can be convenient to writre litteral values like this.")
    expect_identical(
        normalize("
            This is a multi-line string.

            It has an empty line."),
        "This is a multi-line string. It has an empty line.")

    expect_identical(normalize(x1), expected)
    expect_identical(normalize(x2), expected)
})
