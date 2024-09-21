skip("Requires major refactoring and additional tests")

# Load mock source headers in the current testing environment.
source(get_mock_path("tsf-v1-headers"), environment())


# extract_src_header() ---------------------------------------------------------


test_that("extract_src_header() returns a character vector", {
    test_src_head <- extract_src_header(mock_src_header_v1_minimal)

    expect_type(test_src_head, "character")
    expect_length(test_src_head, 7L)
})

test_that("extract_src_header() returns relevant source yaml lines", {
    expected <- c(
        "template_version: 1",
        "generated_by: R package transltr 0.0.1",
        "generated_on: August 22, 2024 @ 08:00 (UTC)",
        "hash_algorithm: blake2b",
        "hash_length: 32",
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
    test_comment    <- test_src_head[[2L]]

    expect_identical(test_n_comments, 1L)
    expect_identical(
        test_comment,
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
    test_head <- from_src_header(extract_src_header(mock_src_header_v1_minimal))

    expect_type(test_head, "list")
    expect_named(test_head)
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


# from_src_header_v1() ---------------------------------------------------------


test_that("from_src_header_v1() returns a named list", {
    test_head <- from_src_header(extract_src_header(mock_src_header_v1))

    expect_type(test_head, "list")
    expect_length(test_head, 7L)
    expect_identical(test_head$template_version, 1L)
    expect_identical(test_head$generated_by,     "R package transltr 0.0.1")
    expect_identical(test_head$generated_on,     "August 22, 2024 @ 08:00 (UTC)")
    expect_identical(test_head$hash_algorithm,   "blake2b")
    expect_identical(test_head$hash_length,      32L)
    expect_identical(test_head$further_fields,   list())
    expect_identical(test_head$language_keys,    c(en = "English", fr = "Français"))
})

test_that("from_src_header_v1() detects and returns further fields", {
    # This test block also checks that from_src_header()
    # passes further fields appropriately via do.call().
    test_src_head <- extract_src_header(mock_src_header_v1_with_further_fields)
    test_head     <- from_src_header(test_src_head)
    test_further_fields <- test_head$further_fields

    expect_length(test_further_fields, 2L)
    expect_identical(test_further_fields$project, "transltr")
    expect_identical(
        test_further_fields$description,
        "An example of a translation Markdown file (version 1)")
})

test_that("from_src_header_v1() validates argument generated_by", {
    expect_error(from_src_header_v1(generated_by = 1L))

    expect_snapshot(from_src_header_v1(generated_by = 1L), error = TRUE)
})

test_that("from_src_header_v1() validates argument generated_on", {
    expect_error(from_src_header_v1(generated_on = 1L))

    expect_snapshot(from_src_header_v1(generated_on = 1L), error = TRUE)
})

test_that("from_src_header_v1() validates argument hash_algorithm", {
    expect_error(from_src_header_v1(hash_algorithm = 1L))

    expect_snapshot(from_src_header_v1(hash_algorithm = 1L), error = TRUE)
})

test_that("from_src_header_v1() validates argument hash_length", {
    expect_error(from_src_header_v1(hash_length = ""))
    expect_error(from_src_header_v1(hash_length = 1L))

    expect_snapshot(from_src_header_v1(hash_length = ""), error = TRUE)
    expect_snapshot(from_src_header_v1(hash_length = 1L), error = TRUE)
})

test_that("from_src_header_v1() validates argument language_keys", {
    expect_error(from_src_header_v1(language_keys = 1L))
    expect_error(from_src_header_v1(language_keys = "English"))

    expect_snapshot(from_src_header_v1(language_keys = 1L),        error = TRUE)
    expect_snapshot(from_src_header_v1(language_keys = "English"), error = TRUE)
})

test_that("from_src_header_v1() validates further fields' names", {
    expect_error(from_src_header_v1(
        template_version = 1L,
        generated_by     = "R package transltr 0.0.1",
        generated_on     = "August 22, 2024 @ 08:00 (UTC)",
        hash_algorithm   = "blake2b",
        hash_length      = 32L,
        language_keys    = list(en = "English", fr = "Français"),
        # We insert a further unnamed field
        # below to generate an error.
        1L))

    expect_snapshot(error = TRUE, {
        from_src_header_v1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm   = "blake2b",
            hash_length      = 32L,
            language_keys    = list(en = "English", fr = "Français"),
            # We insert a further unnamed field
            # below to generate an error.
            1L)
    })
})
