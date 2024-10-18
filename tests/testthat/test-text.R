# text_normalize() -------------------------------------------------------------


test_that("text_normalize() returns a character string", {
    out <- text_normalize("a", "b", "c")
    expect_type(out, "character")
    expect_length(out, 1L)
})

test_that("text_normalize() concatenates argument ...", {
    expect_identical(text_normalize("a", "b", "c",          .concat = ""), "abc")
    expect_identical(text_normalize("a", c("b"), list("c"), .concat = ""), "abc")
})

test_that("text_normalize() removes leading new lines and/or spaces", {
    str <- c(
        "\n This is a leading new line.",
        "  These are leading spaces.",
        "  \t\n These are both.")
    expect_identical(
        text_normalize(str, .concat = "@@"),
        "This is a leading new line.@@These are leading spaces.@@These are both.")
})

test_that("text_normalize() removes trailing spaces", {
    str <- c(
        "This is a trailing new line.\n",
        "These are trailing spaces.  \t ",
        "These are both. \t ")
    expect_identical(
        text_normalize(str, .concat = "@@"),
        "This is a trailing new line.\n@@These are trailing spaces.@@These are both.")
})

test_that("text_normalize() does not remove trailing new lines", {
    expect_identical(
        text_normalize("This is a trailing new line.\n"),
        "This is a trailing new line.\n")
})

test_that("text_normalize() replaces many space characters by a single one", {
    str <- c(
        "This sentence contains many    spaces and   \t tabs.",
        "\t  This sentence contains leading and trailing spaces and tabs.  \t ")
    expect_identical(
        text_normalize(str, .concat = "@@"),
        "This sentence contains many spaces and tabs.@@This sentence contains leading and trailing spaces and tabs.")
})

test_that("text_normalize() removes spaces that immediately follow new lines", {
    str <- c("This is\n      a sentence.")
    expect_identical(text_normalize(str), "This is\na sentence.")
})


# text_hash() ------------------------------------------------------------------


test_that("text_hash() returns a character string", {
    out_sha1 <- text_hash("en", "Hello, world!", "sha1")
    out_utf8 <- text_hash("en", "Hello, world!", "utf8")
    expect_null(text_hash("en", "Hello, world!", "error"))
    expect_type(out_sha1, "character")
    expect_type(out_utf8, "character")
    expect_length(out_sha1, 1L)
    expect_length(out_utf8, 1L)
})

test_that("text_hash() returns a sha-1 hash wheen algorithm is sha1", {
    # Expected SHA-1 hashes were generated externally
    # by https://codebeautify.org/sha1-hash-generator and
    # double-checked by https://10015.io/tools/sha1-encrypt-decrypt.

    long_string <- text_normalize(.multi = " ", "
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
    expect_identical(text_hash("jp", "こんにちは世界！", "sha1"), "422a3da869a9744a7154f713affd25b73bb23c34")
})

test_that("text_hash() returns an integer hash wheen algorithm is utf8", {
    expect_identical(text_hash("en", "Hello, world!", "utf8"), "12351")
})
