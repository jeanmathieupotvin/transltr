mock_file_utf8_path  <- get_mock_path(file.path("text-io", "utf8"),  "txt")
mock_file_eucjp_path <- get_mock_path(file.path("text-io", "eucjp"), "txt")

# Below, 8 non-ASCII characters are written. These are purposely
# non-standard and require a multi-byte encoding. They represent
# a mix of Hiragana and Kanji characters (stemming from Japanese
# syllabaries) and non-standard punctuation symbols. When using
# UTF-8, they all require 3 bytes (each) and therefore, 24 bytes
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

# text_read() ------------------------------------------------------------------

test_that("text_read() returns a character vector", {
    lines <- text_read(mock_file_utf8_path)

    expect_type(lines, "character")
    expect_length(lines, 51L)
})

test_that("text_read() validates path", {
    # Sys.chmod() is incompatible with Windows,
    # but is required for testing purposes. See
    # doc for more information.
    skip_on_os("windows")

    temp_dir  <- withr::local_tempdir(pattern  = "a-test-directory")
    temp_file <- withr::local_tempfile(pattern = "a-non-readable-file")

    # Create file and change permissions.
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
    # Sys.chmod() is incompatible with Windows,
    # but is required for testing purposes. See
    # doc for more information.
    skip_on_os("windows")

    temp_dir  <- withr::local_tempdir(pattern  = "a-test-directory")
    temp_file <- withr::local_tempfile(pattern = "a-non-writable-file")

    # Create file and change permissions.
    # 444 = read-only for owner / group / other.
    # It can still be deleted afterwards.
    file.create(temp_file)
    Sys.chmod(temp_file, "444")

    expect_error(text_write("Hello, world!", 1L))
    expect_error(text_write("Hello, world!", temp_dir))
    expect_error(text_write("Hello, world!", temp_file))
    expect_snapshot(text_write("Hello, world!", 1L),       error = TRUE)
    expect_snapshot({
            "Input temporary directory is random. It is replaced"
            "by a constant for in this snapshot for reproducibility."
            text_write("Hello, world!", temp_dir)
        },
        error     = TRUE,
        transform = \(x) gsub("'path' '(.*?)'", "'path' 'a-test-directory'", x))
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
