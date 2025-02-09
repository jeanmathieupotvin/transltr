test_that("it returns a character string", {
    out <- normalize("a", "b", "c")

    expect_identical(normalize(), "")
    expect_type(out, "character")
    expect_length(out, 1L)
})

test_that("it discards NA values and empty strings", {
    expect_identical(normalize("a", ""), "a")
    expect_identical(normalize("", "a"), "a")
    expect_identical(normalize("a", NA_character_), "a")
    expect_identical(normalize(NA_character_, "a"), "a")
})

test_that("it concatenates ... with the standard paragraph separator", {
    expect_identical(normalize("a", "b", "c"), "a\n\nb\n\nc")
})

test_that("it replaces whitespaces by a single space", {
    str1 <- c(" ", "  ", "\n", "\n\n", "\t", "\t\t", " \n", " \t", "\n\t")
    str2 <- c(
        "Many spaces\nand    \t tabs. ",
        "Many \t\t  tabs and spaces. ",
        "Many tabs and\n\t newlines.")

    expect_identical(normalize(str1), "")
    expect_identical(
        normalize(str2),
        "Many spaces and tabs.\n\nMany tabs and spaces.\n\nMany tabs and newlines.")
})

test_that("it removes leading and trailing whitespaces", {
    str <- c(
        "\n\t  Leading whitespaces. ",
        "Trailing whitespaces.\n\t  ")

    expect_identical(
        normalize(str),
         "Leading whitespaces.\n\nTrailing whitespaces.")
})

test_that("it works as expected on plausible real cases", {
    str1 <- "
        Lorem Ipsum is simply dummy text of the printing and typesetting industry.

        Lorem Ipsum has been the industry's standard dummy text ever since the
        1500s, when an unknown printer    took a galley of type and scrambled it
        to make a type specimen book."

    str2 <- "
        Lorem Ipsum is simply dummy text of the printing and typesetting industry.

            Lorem Ipsum has been the industry's standard dummy text ever
            since the 1500s, when an unknown printer took a galley of type
            and scrambled it to make a type specimen book."

    str3 <- "
        Lorem Ipsum is simply dummy text of the printing and typesetting industry.
        Lorem Ipsum has been the industry's standard dummy text ever since the
        1500s, when an unknown printer took a galley of type and scrambled it
        to make a type specimen book.

        It has survived not only five centuries, but also the leap into
        electronic typesetting, remaining essentially unchanged.

        It was popularised in the 1960s with the release of Letraset sheets
        containing Lorem Ipsum passages, and more recently with desktop
        publishing software like Aldus PageMaker including versions of Lorem
        Ipsum."

    expected <- paste0(
        "Lorem Ipsum is simply dummy text of the printing and typesetting ",
        "industry.\n\nLorem Ipsum has been the industry's standard dummy text ",
        "ever since the 1500s, when an unknown printer took a galley of ",
        "type and scrambled it to make a type specimen book.")

    expect_identical(normalize(str1), expected)
    expect_identical(normalize(str2), expected)
    expect_identical(normalize(str3), paste0(
        "Lorem Ipsum is simply dummy text of the printing and typesetting ",
        "industry. Lorem Ipsum has been the industry's standard dummy text ",
        "ever since the 1500s, when an unknown printer took a galley of type ",
        "and scrambled it to make a type specimen book.\n\nIt has survived not ",
        "only five centuries, but also the leap into electronic typesetting, ",
        "remaining essentially unchanged.\n\nIt was popularised in the 1960s ",
        "with the release of Letraset sheets containing Lorem Ipsum passages, ",
        "and more recently with desktop publishing software like Aldus ",
        "PageMaker including versions of Lorem Ipsum."))
})
