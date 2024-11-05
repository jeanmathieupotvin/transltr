# text_normalize() -------------------------------------------------------------


test_that("text_normalize() returns a character string", {
    out <- text_normalize("a", "b", "c")
    expect_type(out, "character")
    expect_length(out, 1L)
})

test_that("text_normalize() works as expected", {
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
        text_normalize(x1),
        "Lorem Ipsum is simply dummy text of the printing and typesetting industry.\nLorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.")
    expect_identical(
        text_normalize(x2),
        "Lorem Ipsum is simply dummy text of the printing and typesetting industry.\nLorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.")
    expect_identical(
        text_normalize(x3),
        "Lorem Ipsum is simply dummy text of the printing and typesetting industry.\nLorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.")
    expect_snapshot({
        cat(x1, "\n")
        cat(text_normalize(x1), "\n")
    })
    expect_snapshot({
        cat(x2, "\n")
        cat(text_normalize(x2), "\n")
    })
    expect_snapshot({
        cat(x3, "\n")
        cat(text_normalize(x3), "\n")
    })
})

test_that("text_normalize() removes implicit new lines and spaces used for indentation", {
    # This covers step 1 in documentation.
    expect_identical(
        text_normalize("
            This is a multi-line string.
            It is sometimes convenient."),
        "This is a multi-line string. It is sometimes convenient.")
    expect_identical(
        text_normalize("
            This is a multi-line string.
            It has an empty line.

            It is sometimes convenient."),
        "This is a multi-line string. It has an empty line.\nIt is sometimes convenient.")
})

test_that("text_normalize() replaces all non-trailing/leading empty values by a new line", {
    # This covers step 2 in documentation.
    expect_identical(text_normalize("", "a", "", "b", "c"), "a\nb c")
    expect_identical(text_normalize("", "a", "", "b", "c", .concat = ""), "a\nbc")
})

test_that("text_normalize() concatenates values passed to ...", {
    # This covers step 3 in documentation.
    expect_identical(text_normalize("a", "b", "c"),     "a b c")
    expect_identical(text_normalize("a", c("b", "c")),  "a b c")
    expect_identical(text_normalize("a", "b", "c",    .concat = ""), "abc")
    expect_identical(text_normalize("a", c("b", "c"), .concat = ""), "abc")
})

test_that("text_normalize() removes leading new lines and/or spaces", {
    # This partially covers step 4 in documentation.
    str <- c(
        "\nLeading new line.",
        "  Leading spaces.",
        "  \t\nLeading spaces and new lines.")
    expect_identical(
        text_normalize(str, .concat = ""),
         "Leading new line.Leading spaces.Leading spaces and new lines.")
})

test_that("text_normalize() removes trailing new lines and/or spaces", {
    # This partially covers step 4 in documentation.
    # Using \t\t is equivalent to using " " because
    # any substring of space characters is replaced
    # by " ".
    str <- c(
        "Trailing new line.\n",
        "Trailing spaces.  \t ",
        "Trailing spaces and new lines. \t ")
    expect_identical(
        text_normalize(str, .concat = ""),
        "Trailing new line.Trailing spaces.Trailing spaces and new lines.")
    expect_identical(
        text_normalize(str, .concat = "\t\t"),
        "Trailing new line. Trailing spaces. Trailing spaces and new lines.")
})

test_that("text_normalize() replaces many space characters by a single space", {
    # This covers step 5 in documentation.
    str <- c(
        "Many spaces and    \t tabs.",
        "Many \t\t  tabs and spaces.")
    expect_identical(
        text_normalize(str),
        "Many spaces and tabs. Many tabs and spaces.")
})


# text_hash() ------------------------------------------------------------------


test_that("text_hash() returns a character string", {
    out_sha1 <- text_hash("en", "Hello, world!", "sha1")
    out_utf8 <- text_hash("en", "Hello, world!", "utf8")
    expect_type(out_sha1, "character")
    expect_type(out_utf8, "character")
    expect_length(out_sha1, 1L)
    expect_length(out_utf8, 1L)
})

test_that("text_hash() returns null for unknown hashing algorithms", {
    expect_null(text_hash("en", "Hello, world!", "error"))
})

test_that("text_hash() returns a sha-1 hash wheen algorithm is sha1", {
    # Expected SHA-1 hashes were generated externally
    # by https://codebeautify.org/sha1-hash-generator and
    # double-checked by https://10015.io/tools/sha1-encrypt-decrypt.

    long_string <- text_normalize("
        Lorem Ipsum is simply dummy text of the printing and typesetting industry.
        Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,
        when an unknown printer took a galley of type and scrambled it to make a type
        specimen book. It has survived not only five centuries, but also the leap into
        electronic typesetting, remaining essentially unchanged. It was popularised in
        the 1960s with the release of Letraset sheets containing Lorem Ipsum passages,
        and more recently with desktop publishing software like Aldus PageMaker
        including versions of Lorem Ipsum.")

    expect_identical(text_hash("en", long_string,       "sha1"), "ce06fb763aad57fd2b265a18a375d7daae86af7f")
    expect_identical(text_hash("en", "Hello, world!",   "sha1"), "256e0d707386d0fcd9abf10ad994000bdaa25812")
    expect_identical(text_hash("fr", "Bonjour, monde!", "sha1"), "f3c8754329c1b152887d35f00119fca783243d27")
    expect_identical(text_hash("es", "¡Hola Mundo!",    "sha1"), "faf516ddb9969506f4a8771d528efb029db50698")
    expect_identical(text_hash("ja", "こんにちは世界！", "sha1"), "83daa9cda6da5189dc5c81c78323361fab6b652b")
})

test_that("text_hash() returns an integer hash wheen algorithm is utf8", {
    expect_identical(text_hash("en", "Hello, world!", "utf8"), "12351")
})
