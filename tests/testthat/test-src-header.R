# Load mock source headers in the current testing environment.
current_wd <- if (is_testing()) "." else file.path("tests", "testthat")
source(file.path(current_wd, "_mocks", "src-header.R"), environment())


# extract_src_header() ---------------------------------------------------------


test_that("extract_src_header() returns a character vector", {
    test_src_head_v1_minimal <- extract_src_header(mock_src_header_v1_minimal)

    expect_type(test_src_head_v1_minimal, "character")
    expect_length(test_src_head_v1_minimal, 9L)
})

test_that("extract_src_header() returns relevant source yaml lines", {
    expected <- c(
        "template_version: 1",
        "generated_by: R package transltr 0.0.1",
        "generated_on: August 22, 2024 @ 08:00 (UTC)",
        "hash_algorithm: blake2b",
        "hash_length: 32",
        "hashes:",
        "    - \"60ed1cd2b78a3448e7fab38d5830e249\"",
        "language_keys:",
        "    en: English")

    expect_identical(extract_src_header(mock_src_header_v1_minimal), expected)
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

    expect_snapshot(extract_src_header(mock_src_header_no_sep_start), error = TRUE)
    expect_snapshot(extract_src_header(mock_src_header_no_sep_end),   error = TRUE)
})

test_that("extract_src_header() throws an error if header is missing", {
    expect_error(extract_src_header(mock_src_header_no_header))

    expect_snapshot(extract_src_header(mock_src_header_no_header), error = TRUE)
})


# from_src_header() ------------------------------------------------------------


test_that("from_src_header() returns a named list for template version 1", {
    test_head_v1_minimal <- mock_src_header_v1_minimal |>
        extract_src_header() |>
        from_src_header()

    expect_type(test_head_v1_minimal, "list")
    expect_named(test_head_v1_minimal)
})

test_that("from_src_header() throws an error if it detects duplicated yaml map keys", {
    test_src_head <- extract_src_header(mock_src_header_duplicated_map_key)

    expect_error(from_src_header(test_src_head))
    expect_snapshot(from_src_header(test_src_head), error = TRUE)
})

test_that("from_src_header() throws an error if template_version is missing", {
    test_src_head <- extract_src_header(mock_src_header_no_template_version)

    expect_error(from_src_header(test_src_head))
    expect_snapshot(from_src_header(test_src_head), error = TRUE)
})


# from_src_header_version_1() --------------------------------------------------


test_that("from_src_header_version_1() returns a named list", {
    test_head_v1 <- from_src_header(extract_src_header(mock_src_header_v1))

    expect_type(test_head_v1, "list")
    expect_length(test_head_v1, 8L)
    expect_identical(test_head_v1$template_version, 1L)
    expect_identical(test_head_v1$generated_by, "R package transltr 0.0.1")
    expect_identical(test_head_v1$generated_on, "August 22, 2024 @ 08:00 (UTC)")
    expect_identical(test_head_v1$further_fields, list())
    expect_identical(test_head_v1$hash_algorithm, "blake2b")
    expect_identical(test_head_v1$hash_length, 32L)
    expect_identical(test_head_v1$hashes, c(
        "60ed1cd2b78a3448e7fab38d5830e249",
        "9bbbb7410fa6464a1a6a216919179455"))
    expect_identical(test_head_v1$language_keys, c(
        en = "English",
        fr = "Français"))
})

test_that("from_src_header_version_1() detects and returns further fields", {
    # This test block also checks that from_src_header()
    # passes further fields appropriately via do.call().
    further_fields <- mock_src_header_v1_with_further_fields |>
        extract_src_header() |>
        from_src_header() |>
        getElement("further_fields")

    expect_length(further_fields, 2L)
    expect_identical(further_fields$project, "transltr")
    expect_identical(
        further_fields$description,
        "An example of a translation Markdown file (version 1)")
})

test_that("from_src_header_version_1() validates argument generated_by", {
    expect_error(from_src_header_version_1(generated_by = 1L))

    expect_snapshot(from_src_header_version_1(generated_by = 1L), error = TRUE)
})

test_that("from_src_header_version_1() validates argument generated_on", {
    expect_error(from_src_header_version_1(generated_on = 1L))

    expect_snapshot(from_src_header_version_1(generated_on = 1L), error = TRUE)
})

test_that("from_src_header_version_1() validates argument hash_algorithm", {
    expect_error(from_src_header_version_1(hash_algorithm = 1L))

    expect_snapshot(from_src_header_version_1(hash_algorithm = 1L), error = TRUE)
})

test_that("from_src_header_version_1() validates argument hash_length", {
    expect_error(from_src_header_version_1(hash_length = ""))
    expect_error(from_src_header_version_1(hash_length = 1L))

    expect_snapshot(from_src_header_version_1(hash_length = ""), error = TRUE)
    expect_snapshot(from_src_header_version_1(hash_length = 1L), error = TRUE)
})

test_that("from_src_header_version_1() validates argument hashes", {
    expect_error(from_src_header_version_1(hashes = 1L))
    expect_error(
        from_src_header_version_1(
            hash_algorithm = "blake2b",
            hash_length    = 8L,
            hashes         = "a"))

    expect_snapshot(from_src_header_version_1(hashes = 1L), error = TRUE)
    expect_snapshot(error = TRUE, {
        from_src_header_version_1(
            hash_algorithm = "blake2b",
            hash_length    = 8L,
            hashes         = "a")
    })
})

test_that("from_src_header_version_1() validates argument language_keys", {
    expect_error(from_src_header_version_1(language_keys = 1L))
    expect_error(from_src_header_version_1(language_keys = "English"))

    expect_snapshot(from_src_header_version_1(language_keys = 1L),        error = TRUE)
    expect_snapshot(from_src_header_version_1(language_keys = "English"), error = TRUE)
})

test_that("from_src_header_version_1() validates further fields' names", {
    expect_error(from_src_header_version_1(
        template_version = 1L,
        generated_by     = "R package transltr 0.0.1",
        generated_on     = "August 22, 2024 @ 08:00 (UTC)",
        language_keys    = list(en = "English", fr = "Français"),
        hash_algorithm   = "blake2b",
        hash_length      = 32L,
        hashes           = c(
            "60ed1cd2b78a3448e7fab38d5830e249",
            "9bbbb7410fa6464a1a6a216919179455"),
        1L))

    expect_snapshot(error = TRUE, {
        from_src_header_version_1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            language_keys    = list(en = "English", fr = "Français"),
            hash_algorithm   = "blake2b",
            hash_length      = 32L,
            hashes           = c(
                "60ed1cd2b78a3448e7fab38d5830e249",
                "9bbbb7410fa6464a1a6a216919179455"),
            1L)
    })
})
