mock_file_utf8_path  <- get_mock_path("io-text-utf8",  "txt")
mock_file_eucjp_path <- get_mock_path("io-text-eucjp", "txt")

# Below, 8 non-ASCII characters are written. These are purposely
# non-standard and require a multi-byte encoding. They represent
# a mix of Hiragana and Kanji characters (stemming from Japanese
# syllabaries), and non-standard punctuation symbols. When using
# UTF-8, they all require 3 bytes (each), and therefore, 24 bytes
# should be written to match expectations (nchar(,"bytes") should
# return 24). The expected byte sequences were extracted from
# https://symbl.cc/.
chars     <- enc2utf8("さらば、世界よ！")
chars_len <- nchar(chars, "bytes")
chars_raw <- as.raw(c(
    0xe3, 0x81, 0x95,  # さ  (HIRAGANA LETTER SA)
    0xe3, 0x82, 0x89,  # ら  (HIRAGANA LETTER RA)
    0xe3, 0x81, 0xb0,  # ば  (HIRAGANA LETTER BA)
    0xe3, 0x80, 0x81,  # 、  (IDEOGRAPHIC COMMA)
    0xe4, 0xb8, 0x96,  # 世  (KANJI -> IDEOGRAPH GENERATION; WORLD; ERA CJK)
    0xe7, 0x95, 0x8c,  # 界  (KANJI -> IDEOGRAPH BOUNDARY, LIMIT; DOMAIN; SOCIETY; THE WORLD CJK)
    0xe3, 0x82, 0x88,  # よ  (HIRAGANA LETTER YO)
    0xef, 0xbc, 0x81)) # ！  (FULLWIDTH EXCLAMATION MARK)


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


# text_read() ------------------------------------------------------------------


test_that("text_read() returns a character vector", {
    lines <- text_read(mock_file_utf8_path)

    expect_type(lines, "character")
    expect_length(lines, 51L)
})

test_that("text_read() validates path", {
    temp_dir  <- withr::local_tempdir(pattern  = "a-test-directory")
    temp_file <- withr::local_tempfile(pattern = "a-non-readable-file")

    # Create file, and change permissions.
    # 222 = write-only for owner / group / other.
    # It can still be deleted afterwards.
    file.create(temp_file)
    Sys.chmod(temp_file, "222")

    expect_error(text_read(1L))
    expect_error(text_read("a-non-existent-file.txt"))
    expect_error(text_read(temp_dir))
    expect_error(text_read(temp_file))
    expect_snapshot(text_read(1L), error = TRUE)
    expect_snapshot(text_read("a-non-existent-file.txt"), error = TRUE)
})

test_that("text_read() validates encoding", {
    expect_error(text_read(mock_file_utf8_path, ""))
    expect_snapshot(text_read(mock_file_utf8_path, ""), error = TRUE)
})

test_that("text_read() closes connection to path", {
    # Register number of opened connections just
    # before execution of text_read(). It should
    # be the same after execution.
    n_cons <- nrow(showConnections())
    text_read(mock_file_utf8_path)

    expect_identical(nrow(showConnections()), n_cons)
})

test_that("text_read() re-encodes input to utf-8", {
    lines <- text_read(mock_file_eucjp_path, "EUC-JP")
    chars <- utils::tail(lines, 1L)

    expect_identical(chars, "あいうえお")
    expect_identical(Encoding(chars), "UTF-8")
})


# text_write() -----------------------------------------------------------------


test_that("text_write() returns null", {
    temp_file <- withr::local_tempfile()

    expect_null(text_write("Hello, world!", temp_file))
})

test_that("text_write() validates x", {
    expect_error(text_write(1L))
    expect_snapshot(text_write(1L), error = TRUE)
})

test_that("text_write() validates path", {
    temp_dir  <- withr::local_tempdir(pattern  = "a-test-directory")
    temp_file <- withr::local_tempfile(pattern = "a-non-writable-file")

    # Create file, and change permissions.
    # 444 = read-only for owner / group / other.
    # It can still be deleted afterwards.
    file.create(temp_file)
    Sys.chmod(temp_file, "444")

    expect_error(text_write("Hello, world!", 1L))
    expect_error(text_write("Hello, world!", temp_dir))
    expect_error(text_write("Hello, world!", temp_file))
    expect_snapshot(text_write("Hello, world!", 1L),       error = TRUE)
    expect_snapshot(text_write("Hello, world!", temp_dir), error = TRUE)
})

test_that("text_write() validates encoding", {
    temp_file <- withr::local_tempfile()

    expect_error(text_write("Hello, world!", temp_file, 1L))
    expect_snapshot(text_write("Hello, world!", temp_file, 1L), error = TRUE)
})

test_that("text_write() closes connection to path", {
    # Register number of opened connections just
    # before execution of text_read(). It should
    # be the same after execution.
    n_cons    <- nrow(showConnections())
    temp_file <- withr::local_tempfile()
    text_write("Hello, world!", temp_file)

    expect_identical(nrow(showConnections()), n_cons)
})

test_that("text_write() writes utf-8 character to path", {
    temp_file <- withr::local_tempfile()
    text_write(chars, temp_file)

    expect_identical(readBin(temp_file, "raw", n = chars_len), chars_raw)
    expect_identical(text_read(temp_file), chars)
})

test_that("text_write() re-encodes input to utf-8", {
    temp_file   <- withr::local_tempfile()
    chars_eucjp <- iconv(chars, from = "UTF-8", to = "EUC-JP")
    text_write(chars_eucjp, temp_file, "EUC-JP")

    expect_identical(readBin(temp_file, "raw", n = chars_len), chars_raw)
    expect_identical(text_read(temp_file), chars)
})
