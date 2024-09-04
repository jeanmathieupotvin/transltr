# Current working directory is updated by testthat::test_*() functions.
test_dir       <- if (is_testing()) "." else file.path("tests", "testthat")
test_mock_file <- file.path(test_dir, "_mocks", "src-header.R")

# Load mock source headers in the current testing environment.
source(test_mock_file, environment())

test_src_head_v1_minimal <- extract_src_header(mock_src_header_v1_minimal)


# extract_src_header() ---------------------------------------------------------


test_that("extract_src_header() returns a character vector", {
    test_src_head <- test_src_head_v1_minimal

    expect_type(test_src_head, "character")
    expect_length(test_src_head, 11L)
})

test_that("extract_src_header() returns relevant source yaml lines", {
    expected <- c(
        "template_version: 1",
        "generated_by: R package transltr 0.0.1",
        "generated_on: August 22, 2024 @ 08:00 UTC",
        "hash_algorithm: blake2b",
        "hash_length: 32",
        "hashes:",
        "    - \"60ed1cd2b78a3448e7fab38d5830e249\"",
        "language_keys:",
        "    en: English")

    expect_identical(test_src_head_v1_minimal, expected)
})

test_that("extract_src_header() handles yaml comments", {
    # The very first comment of mock_src_header_with_comments
    # is placed outside of the pairs of separators. Therefore,
    # it should be discarded.
    test_src_head   <- extract_src_header(mock_src_header_with_comments)
    test_n_comments <- length(grep("^\\#", test_src_head))

    expect_identical(test_n_comments, 1L)
    expect_identical(
        extract_src_header(mock_src_header_with_comments)[[2L]],
        "# A comment that may give further details on a field")
})

test_that("extract_src_header() throws an error if a separator is missing", {
    expect_error(extract_src_header(mock_src_header_no_sep_start))
    expect_error(extract_src_header(mock_src_header_no_sep_end))

    expect_snapshot(
        extract_src_header(mock_src_header_no_sep_start),
        error = TRUE)
    expect_snapshot(
        extract_src_header(mock_src_header_no_sep_end),
        error = TRUE)
})

test_that("extract_src_header() throws an error if header is missing", {
    expect_error(extract_src_header(mock_src_header_no_header))

    expect_snapshot(
        extract_src_header(mock_src_header_no_header),
        error = TRUE)
})

test_that("extract_src_header() throws an error if it detects duplicated yaml map keys", {
    # TODO: here.
})


# from_src_header() ------------------------------------------------------------


# TODO: here.
