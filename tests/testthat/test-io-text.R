mock_file_utf8_path  <- get_mock_path("io-text-utf8",  "txt")
mock_file_eucjp_path <- get_mock_path("io-text-eucjp", "txt")


# read_text() ------------------------------------------------------------------


test_that("read_text() returns a character vector", {
    lines <- read_text(mock_file_utf8_path)

    expect_type(lines, "character")
    expect_length(lines, 51L)
})

test_that("read_text() validates path", {
    # Creates a directory within tempdir() and
    # an empty file within this subdirectory.
    temp_dir  <- withr::local_tempdir(pattern  = "a-test-directory")
    temp_file <- withr::local_tempfile(pattern = "a-non-readable-file")

    # Mark the file as being a binary executable.
    # This makes it non-readable.
    Sys.chmod(temp_file, "711")

    expect_error(read_text(1L))
    expect_error(read_text("a-non-existent-file.txt"))
    expect_error(read_text(temp_dir))
    expect_error(read_text(temp_file))

    expect_snapshot(read_text(1L), error = TRUE)
    expect_snapshot(read_text("a-non-existent-file.txt"), error = TRUE)
})

test_that("read_text() validates encoding", {
    expect_error(read_text(mock_file_utf8_path, ""))
    expect_snapshot(read_text(mock_file_utf8_path, ""), error = TRUE)
})

test_that("read_text() closes connection to file", {
    # Register number of opened connections just
    # before execution of read_text(). It should
    # be the same after execution.
    n_cons <- nrow(showConnections())
    read_text(mock_file_utf8_path)

    expect_identical(nrow(showConnections()), n_cons)
})

test_that("read_text() re-encodes input to utf-8", {
    lines <- read_text(mock_file_eucjp_path, "EUC-JP")
    chars <- utils::tail(lines, 1L)

    expect_identical(chars, "あいうえお")
    expect_identical(Encoding(chars), "UTF-8")
})


# write_text() -----------------------------------------------------------------


test_that("write_text() returns a not yet implemented error", {
    expect_error(write_text())
})
