test_that("it returns a character string", {
    out <- normalize("a", "b", "c")
    expect_type(out, "character")
    expect_length(out, 1L)
})

test_that("it validates concat", {
    expect_error(normalize(concat = 1L))
    expect_snapshot(normalize(concat = 1L), error = TRUE)
})

test_that("it works as expected", {
    # This covers exaggerated expected use cases extracted
    # from documentation. The format of constants below is
    # intentional and must be left as is.
    x1 <- "
        Lorem Ipsum is simply dummy text of the printing and typesetting industry.

        Lorem Ipsum has been the industry's standard dummy text ever since the
        1500s, when an unknown printer    took a galley of type and scrambled it
        to make a type specimen book."

    x2 <- c(
        "",
        "Lorem Ipsum is simply dummy text of the printing and typesetting",
        "industry.",
        "",
        "Lorem Ipsum has been the industry's standard dummy text ever since",
        "the 1500s, when an unknown printer    took a galley of type and",
        "scrambled it to make a type specimen book.")

    x3 <- "
        Lorem Ipsum is simply dummy text of the printing and typesetting industry.

            Lorem Ipsum has been the industry's standard dummy text ever
            since the 1500s, when an unknown printer took a galley of type
            and scrambled it to make a type specimen book. It has survived
            not only five centuries, but also the leap into electronic
            typesetting, remaining essentially unchanged. It was popularised
            in the 1960s with the release of Letraset sheets containing Lorem
            Ipsum passages, and more recently with desktop publishing software
            like Aldus PageMaker including versions of Lorem Ipsum."

    expect_identical(
        normalize(x1),
        "Lorem Ipsum is simply dummy text of the printing and typesetting industry.\nLorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.")
    expect_identical(
        normalize(x2),
        "Lorem Ipsum is simply dummy text of the printing and typesetting industry.\nLorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.")
    expect_identical(
        normalize(x3),
        "Lorem Ipsum is simply dummy text of the printing and typesetting industry.\nLorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.")
    expect_snapshot({
        cat(x1, "\n")
        cat(normalize(x1), "\n")
    })
    expect_snapshot({
        cat(x2, "\n")
        cat(normalize(x2), "\n")
    })
    expect_snapshot({
        cat(x3, "\n")
        cat(normalize(x3), "\n")
    })
})

test_that("it removes implicit new lines and spaces used for indentation", {
    # This covers step 1 in documentation.
    expect_identical(
        normalize("
            This is a multi-line string.
            It is sometimes convenient."),
        "This is a multi-line string. It is sometimes convenient.")
    expect_identical(
        normalize("
            This is a multi-line string.
            It has an empty line.

            It is sometimes convenient."),
        "This is a multi-line string. It has an empty line.\nIt is sometimes convenient.")
})

test_that("it replaces all non-trailing/leading empty values by a new line", {
    # This covers step 2 in documentation.
    expect_identical(normalize("", "a", "", "b", "c"), "a\nb c")
    expect_identical(normalize("", "a", "", "b", "c", concat = ""), "a\nbc")
})

test_that("it concatenates values passed to ...", {
    # This covers step 3 in documentation.
    expect_identical(normalize("a", "b", "c"),     "a b c")
    expect_identical(normalize("a", c("b", "c")),  "a b c")
    expect_identical(normalize("a", "b", "c",    concat = ""), "abc")
    expect_identical(normalize("a", c("b", "c"), concat = ""), "abc")
})

test_that("it removes leading new lines and/or spaces", {
    # This partially covers step 4 in documentation.
    str <- c(
        "\nLeading new line.",
        "  Leading spaces.",
        "  \t\nLeading spaces and new lines.")
    expect_identical(
        normalize(str, concat = ""),
         "Leading new line.Leading spaces.Leading spaces and new lines.")
})

test_that("it removes trailing new lines and/or spaces", {
    # This partially covers step 4 in documentation.
    # Using \t\t is equivalent to using " " because
    # any substring of space characters is replaced
    # by " ".
    str <- c(
        "Trailing new line.\n",
        "Trailing spaces.  \t ",
        "Trailing spaces and new lines. \t ")
    expect_identical(
        normalize(str, concat = ""),
        "Trailing new line.Trailing spaces.Trailing spaces and new lines.")
    expect_identical(
        normalize(str, concat = "\t\t"),
        "Trailing new line. Trailing spaces. Trailing spaces and new lines.")
})

test_that("it replaces many space characters by a single space", {
    # This covers step 5 in documentation.
    str <- c(
        "Many spaces and    \t tabs.",
        "Many \t\t  tabs and spaces.")
    expect_identical(
        normalize(str),
        "Many spaces and tabs. Many tabs and spaces.")
})
