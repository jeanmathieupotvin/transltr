#' Testing from_tsf()
#'
#' @details
#' This function leverage many lower-level functions all closely tied and meant
#' to be used within a certain hierarchy already explained in R/from-tsf.R. The
#' tests below indirectly leverage this hierarchy and assume input(s) stemming
#' from higher levels are valid. They strictly test the logic they encapsulate
#' and therefore, distinct test_that() expressions may complement each other.
#'
#' For the sake of readability and conciseness, some parts of the script break
#' the usual rule of thumb of keeping lines shorter than 80 characters.
#'
#' For simplicity and whenever possible, tsf_block_line_token() only assigns a
#' value (and never a type or subtype). This is because many subfunctions only
#' use the value. Doing so keeps the logic more succinct and lets developers
#' focus on what matters for testing purposes.
NULL


mock_tsf_v1                  <- text_read(get_mock_path("tsf-v1", "md"))
mock_tsf_v1_head             <- text_read(get_mock_path("tsf-v1-head", "md"))
mock_tsf_v1_block_hello      <- text_read(get_mock_path("tsf-v1-block-hello", "md"))
mock_tsf_v1_block_lorem      <- text_read(get_mock_path("tsf-v1-block-lorem", "md"))
mock_tsf_v1_block_lorem_text <- text_read(get_mock_path("tsf-v1-block-lorem-text", "md"))

# _t suffix stands for tokens.
mock_tsf_v1_block_hello_t      <- tokenize_tsf_block_v1(mock_tsf_v1_block_hello)
mock_tsf_v1_block_lorem_t      <- tokenize_tsf_block_v1(mock_tsf_v1_block_lorem)
mock_tsf_v1_block_lorem_text_t <- tokenize_tsf_block_v1(mock_tsf_v1_block_lorem_text)

# _p suffix stands for parsed.
mock_tsf_v1_block_lorem_text_p <- from_tsf_block_txt_v1(mock_tsf_v1_block_lorem_text_t)


# from_tsf() -------------------------------------------------------------------


test_that("from_tsf() returns a named list", {
    out <- from_tsf(mock_tsf_v1)

    expect_type(out, "list")
    expect_length(out, 3L)
    expect_named(out, c("header", "blocks", "rest"))
})

test_that("from_tsf() converts translations source files appropriately", {
    out   <- from_tsf(mock_tsf_v1)
    src_b <- list(mock_tsf_v1_block_lorem, mock_tsf_v1_block_hello)

    expect_identical(out$header, from_tsf_header(mock_tsf_v1_head))
    expect_identical(out$blocks, from_tsf_blocks_v1(src_b, "sha1"))
    expect_identical(out$rest,   "")
})


# split_tsf() ------------------------------------------------------------------


test_that("split_tsf() returns a named list", {
    out <- split_tsf(mock_tsf_v1)

    expect_type(out, "list")
    expect_length(out, 3L)
    expect_named(out, c("header", "blocks", "rest"))

})

test_that("split_tsf() splits translations source files appropriately", {
    out <- split_tsf(mock_tsf_v1)

    expect_identical(out$header, mock_tsf_v1_head)
    expect_identical(out$blocks, list(mock_tsf_v1_block_lorem, mock_tsf_v1_block_hello))
    expect_identical(out$rest,   "")
})

test_that("split_tsf() validates x", {
    expect_error(split_tsf(1L))
    expect_snapshot(split_tsf(1L), error = TRUE)
})

test_that("split_tsf() throws an error if a header's separator is missing", {
    # Lines 1 and 22 are the positions of the header's separators.
    mock_tsf_v1_no_sep_start <- mock_tsf_v1[ -1L]
    mock_tsf_v1_no_sep_end   <- mock_tsf_v1[-22L]

    expect_error(split_tsf(mock_tsf_v1_no_sep_start))
    expect_error(split_tsf(mock_tsf_v1_no_sep_end))
    expect_snapshot(split_tsf(mock_tsf_v1_no_sep_start), error = TRUE)
})

test_that("split_tsf() throws an error if the header is missing", {
    # Lines 1 @ 22 are the header.
    mock_tsf_v1_no_head <- mock_tsf_v1[-seq.int(1L, 22L)]

    expect_error(split_tsf(mock_tsf_v1_no_head))
    expect_snapshot(split_tsf(mock_tsf_v1_no_head), error = TRUE)
})


# from_tsf_header() ------------------------------------------------------------


test_that("from_tsf_header() returns a named list for template version 1", {
    # Here we only test whether from_tsf_header()
    # returns the output of from_tsf_header_v1() or not.
    mock_tsf_v1_head <- c(
        "",
        "template_version: 1",
        "generated_by: R package transltr 0.0.1",
        "generated_on: August 22, 2024 @ 08:00 (UTC)",
        "hash_algorithm: sha1",
        "language_keys:",
        "    en: English",
        "    fr: Français",
        "")
    out <- from_tsf_header(mock_tsf_v1_head)

    expect_identical(out,
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm   = "sha1",
            language_keys    = list(en = "English", fr = "Français")))
})

test_that("from_tsf_header() throws an error if template_version is missing", {
    expect_error(from_tsf_header(""))
    expect_snapshot(from_tsf_header(""), error = TRUE)
})

test_that("from_tsf_header() throws an error if the yaml parser fails", {
    # Here we only test whether the tryCatch() block
    # returns the expected error message via .stopf().
    # We do not re-test the YAML engine of package yaml.
    mock_tsf_v1_head_dup_keys <- c(
        "template_version: 1",
        "generated_by: R package transltr 0.0.1",
        "generated_on: August 22, 2024 @ 08:00 (UTC)",
        "hash_algorithm: sha1",
        "language_keys:",
        "    en: English",
        "    en: French")

    expect_error(from_tsf_header(mock_tsf_v1_head_dup_keys))
    expect_snapshot(from_tsf_header(mock_tsf_v1_head_dup_keys), error = TRUE)
})

test_that("from_tsf_header() skips yaml comments", {
    # We check that from_tsf_header() returns the
    # output of from_tsf_header_v1(). This means
    # comments are skipped and does not interfere
    # with the parsing process.
    mock_tsf_v1_head_comments <- c(
        "template_version: 1",
        "# A comment",
        "generated_by: R package transltr 0.0.1",
        "generated_on: August 22, 2024 @ 08:00 (UTC)",
        "hash_algorithm: sha1",
        "# Another comment",
        "language_keys:",
        "    en: English",
        "    fr: Français")

    out <- from_tsf_header(mock_tsf_v1_head_comments)

    expect_identical(out,
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm   = "sha1",
            language_keys    = list(en = "English", fr = "Français")))
})


# from_tsf_blocks() ------------------------------------------------------------


test_that("from_tsf_blocks() returns a list: argument template_version is equal to 1", {
    # We only need to test whether from_tsf_blocks() returns
    # a list containing the output of from_tsf_block_v1() or not.
    out <- from_tsf_blocks(list(mock_tsf_v1_block_hello), 1L)

    expect_type(out, "list")
    expect_length(out, 1L)
    expect_identical(out, list(from_tsf_block_v1(mock_tsf_v1_block_hello_t, "sha1")))
})

test_that("from_tsf_blocks() validates template_version", {
    expect_error(from_tsf_blocks(template_version = ""))
    expect_snapshot(from_tsf_blocks(template_version = ""), error = TRUE)
})


# from_tsf_header_v1() ---------------------------------------------------------


test_that("from_tsf_header_v1() returns a named list and expected fields", {
    header <- from_tsf_header_v1(
        template_version = 1L,
        generated_by     = "R package transltr 0.0.1",
        generated_on     = "August 22, 2024 @ 08:00 (UTC)",
        hash_algorithm   = "sha1",
        language_keys    = list(en = "English", fr = "Français"),
        further_field_1  = "test value 1",
        further_field_2  = "test value 2")

    expect_type(header, "list")
    expect_length(header, 6L)
    expect_identical(header$template_version, 1L)
    expect_identical(header$generated_by,     "R package transltr 0.0.1")
    expect_identical(header$generated_on,     "August 22, 2024 @ 08:00 (UTC)")
    expect_identical(header$hash_algorithm,   "sha1")
    expect_identical(header$language_keys,    c(en = "English", fr = "Français"))
    expect_identical(header$further_fields,   list(
        further_field_1 = "test value 1",
        further_field_2 = "test value 2"))
})

test_that("from_tsf_header_v1() throws an (appropriate) error if a field is missing", {
    expect_error(from_tsf_header_v1())

    expect_error(
        from_tsf_header_v1(
            # template_version = 1L,
            generated_by       = "R package transltr 0.0.1",
            generated_on       = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm     = "sha1",
            language_keys      = list(en = "English", fr = "Français")))
    expect_error(
        from_tsf_header_v1(
            # template_version = 1L,
            # generated_by     = "R package transltr 0.0.1",
            generated_on       = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm     = "sha1",
            language_keys      = list(en = "English", fr = "Français")))
    expect_error(
        from_tsf_header_v1(
            # template_version = 1L,
            # generated_by     = "R package transltr 0.0.1",
            # generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm     = "sha1",
            language_keys      = list(en = "English", fr = "Français")))
    expect_error(
        from_tsf_header_v1(
            # template_version = 1L,
            # generated_by     = "R package transltr 0.0.1",
            # generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            # hash_algorithm   = "sha1",
            language_keys      = list(en = "English", fr = "Français")))
    expect_error(
        from_tsf_header_v1(
            # template_version = 1L,
            # generated_by     = "R package transltr 0.0.1",
            # generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            # hash_algorithm   = "sha1",
            language_keys      = list(en = "English", fr = "Français")))

    # If all expectations above succeeds, we only need to
    # record the "worst case scenario" error message where
    # all fields are missing.
    expect_snapshot(from_tsf_header_v1(), error = TRUE)
})

test_that("from_tsf_header_v1() validates generated_by", {
    expect_error(
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = 1L,
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm   = "sha1",
            language_keys    = list(en = "English", fr = "Français")))
    expect_snapshot(
        error = TRUE,
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = 1L,
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm   = "sha1",
            language_keys    = list(en = "English", fr = "Français")))
})

test_that("from_tsf_header_v1() validates generated_on", {
    expect_error(
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = 1L,
            hash_algorithm   = "sha1",
            language_keys    = list(en = "English", fr = "Français")))
    expect_snapshot(
        error = TRUE,
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = 1L,
            hash_algorithm   = "sha1",
            language_keys    = list(en = "English", fr = "Français")))
})

test_that("from_tsf_header_v1() validates hash_algorithm", {
    expect_error(
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm   = 1L,
            language_keys    = list(en = "English", fr = "Français")))
    expect_snapshot(
        error = TRUE,
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm   = 1L,
            language_keys    = list(en = "English", fr = "Français")))
})

test_that("from_tsf_header_v1() validates language_keys", {
    expect_error(
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm   = "sha1",
            language_keys    = 1L))
    expect_error(
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm   = "sha1",
            language_keys    = list("English", "Français")))
    expect_snapshot(
        error = TRUE,
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm   = "sha1",
            language_keys    = 1L))
    expect_snapshot(
        error = TRUE,
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm   = "sha1",
            language_keys    = list("English", "Français")))
})

test_that("from_tsf_header_v1() validates further fields", {
    expect_error(
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm   = "sha1",
            language_keys    = list(en = "English", fr = "Français"),
            # Insert an unnamed field to generate an error.
            1L))
    expect_snapshot(
        error = TRUE,
        from_tsf_header_v1(
            template_version = 1L,
            generated_by     = "R package transltr 0.0.1",
            generated_on     = "August 22, 2024 @ 08:00 (UTC)",
            hash_algorithm   = "sha1",
            language_keys    = list(en = "English", fr = "Français"),
            # Insert an unnamed field to generate an error.
            1L))
})


# from_tsf_blocks_v1() ---------------------------------------------------------


test_that("from_tsf_blocks_v1() works", {
    src_b <- list(mock_tsf_v1_block_lorem, mock_tsf_v1_block_hello)
    out   <- from_tsf_blocks_v1(src_b)

    expect_type(out, "list")
    expect_length(out, 2L)
    expect_identical(out, list(
        from_tsf_block_v1(mock_tsf_v1_block_lorem_t),
        from_tsf_block_v1(mock_tsf_v1_block_hello_t)))
})


# from_tsf_block_v1() ----------------------------------------------------------


test_that("from_tsf_block_v1() returns a S3 object of class Block", {
    out <- from_tsf_block_v1(mock_tsf_v1_block_hello_t, "sha1")

    expect_s3_class(out, "Block")
    expect_identical(out$hash, "256e0d707386d0fcd9abf10ad994000bdaa25812")
    expect_identical(out$hash_algorithm, "sha1")
    expect_identical(out$source_lang, "en")
    expect_identical(out$source_text, "Hello, world!")
    expect_identical(out$locations, list(file1 = location("file1", 1L, 2L, 3L, 4L)))
    expect_identical(out$translations, c(
        en = "Hello, world!",
        es = "¡Hola Mundo!",
        fr = "Bonjour le monde!",
        ja = "こんにちは世界！"))
})


# from_tsf_block_title_v1() ----------------------------------------------------


test_that("from_tsf_block_txt_v1() returns a character string", {
    expect_identical(
        expected = "h1-title",
        from_tsf_block_title_v1(
            tsf_block_line_token(value = "# `{{ h1-title }}`")))
})

test_that("from_tsf_block_txt_v1() properly tokenizes strings", {
    expect_identical(
        expected = "h2-title",
        from_tsf_block_title_v1(
            tsf_block_line_token(value = "##\t`{{ h2-title }}`")))

    # Individual char tokens are also stripped
    # from the title because in the context of
    # transltr they should not be used.
    expect_identical(
        expected = "h2-title-with--chars",
        from_tsf_block_title_v1(
            tsf_block_line_token(value = "##\t`{{ h2-title-with-#{}`-chars }}`")))
})


# from_tsf_block_txt_v1() ------------------------------------------------------


test_that("from_tsf_block_txt_v1() returns a character string", {
    expect_type(mock_tsf_v1_block_lorem_text_p, "character")
    expect_length(mock_tsf_v1_block_lorem_text_p, 1L)
})

test_that("from_tsf_block_txt_v1() strips leading and trailing empty lines", {
    expect_no_match(mock_tsf_v1_block_lorem_text_p, "^[ \t\n]+")
    expect_no_match(mock_tsf_v1_block_lorem_text_p, "[ \t\n]$")
})

test_that("from_tsf_block_txt_v1() preserves empty lines within paragraphs", {
    # There should be only one \n character at position 79
    # (start of line 4). See original mock for more information.
    expect_identical(
        expected = 79L,
        which(strsplit(mock_tsf_v1_block_lorem_text_p, NULL)[[1L]] == "\n"))
})


# from_tsf_block_loc_v1() ------------------------------------------------------


test_that("from_tsf_block_loc_v1() returns a S3 object of class Location", {
    # We only need to test whether outputs of subfunctions
    # from_tsf_block_loc_path_v1() and from_tsf_block_loc_range_v1()
    # are properly used to create the object.
    tokens <- tokenize_tsf_block_v1(c(
        "`file1`:",
        "  - line  1, column   2 @ line    3, column  4224",
        "  - line 24, column 434 @ line 3421, column 35972"))
    out <- from_tsf_block_loc_v1(tokens)

    expect_s3_class(out, "Location")
    expect_identical(out$path, "file1")
    expect_identical(out$line1, c(1L,       24L))
    expect_identical(out$col1,  c(2L,      434L))
    expect_identical(out$line2, c(3L,     3421L))
    expect_identical(out$col2,  c(4224L, 35972L))
})


# from_tsf_block_loc_path_v1() -------------------------------------------------


test_that("from_tsf_block_loc_path_v1() returns a character string", {
    expect_identical(
        expected = "file",
        from_tsf_block_loc_path_v1(tsf_block_line_token(value = "`file`:")))
})

test_that("from_tsf_block_loc_path_v1() properly tokenizes strings", {
    expect_identical(
        expected = "/normal/path/from/root/to/file.md",
        from_tsf_block_loc_path_v1(
            tsf_block_line_token(
                value = "`/normal/path/from/root/to/file.md`:")))
    expect_identical(
        expected = "relative/path/to/file.md",
        from_tsf_block_loc_path_v1(
            tsf_block_line_token(
                value = "`relative/path/to/file.md`:")))
    expect_identical(
        expected = "https://cdn.domain.com:443/tsfs/my-tsf.md",
        from_tsf_block_loc_path_v1(
            tsf_block_line_token(
                value = "`https://cdn.domain.com:443/tsfs/my-tsf.md`:")))
    expect_identical(
        expected = "C:\\tsfs\\my-tsf.md",
        from_tsf_block_loc_path_v1(
            tsf_block_line_token(
                value = "`C:\\tsfs\\my-tsf.md`:")))
})


# from_tsf_block_loc_range_v1() ------------------------------------------------


test_that("from_tsf_block_loc_range_v1() returns a named integer vector", {
    out <- from_tsf_block_loc_range_v1(
        tsf_block_line_token(
            value = "- line  1, column    2 @ line    3, column  4224"))

    expect_type(out, "integer")
    expect_length(out, 4L)
    expect_identical(out, c(line1 = 1L, col1 = 2L, line2 = 3L, col2 = 4224L))
})

test_that("from_tsf_block_loc_range_v1() properly tokenizes strings", {
    exp <- c(line1 = 1L, col1 = 2L, line2 = 3L, col2 = 4224L)

    # Missing starting dash.
    expect_identical(
        expected = exp,
        from_tsf_block_loc_range_v1(
            tsf_block_line_token(
                value = "line 1, column 2 @ line 3, column 4224")))

    # Standard format.
    expect_identical(
        expected = exp,
        from_tsf_block_loc_range_v1(
            tsf_block_line_token(
                value = "- line 1, column 2 @ line 3, column 4224")))

    # Extra padding for better printing.
    expect_identical(
        expected = exp,
        from_tsf_block_loc_range_v1(
            tsf_block_line_token(
                value = "- line  1, column    2 @ line    3, column  4224")))
})

test_that("from_tsf_block_loc_range_v1() throws an error if there are more than 4 values", {
    expect_error(
        from_tsf_block_loc_range_v1(
            tsf_block_line_token(
                value = "- line  1,24 column 2 @ line 3, column 4")))
    expect_snapshot(
        error = TRUE,
        from_tsf_block_loc_range_v1(
            tsf_block_line_token(
                value = "- line  1,24 column 2 @ line 3, column 4")))
})

test_that("from_tsf_block_loc_range_v1() throws an error if it detects invalid values", {
    expect_error(
        from_tsf_block_loc_range_v1(
            tsf_block_line_token(
                value = "- line  1, column 2+4i @ line 3, column 4")))
    expect_error(
        from_tsf_block_loc_range_v1(
            tsf_block_line_token(
                value = "- line  1, column 2 @ line -3, column 4")))
    expect_snapshot(
        error = TRUE,
        from_tsf_block_loc_range_v1(
            tsf_block_line_token(
                value = "- line  1, column 2+4i @ line 3, column 4")))
})

test_that("from_tsf_block_loc_range_v1() accomodates shorter formats", {
    # This is undocumented to avoid any confusion.
    # However, it is better and useful to have a
    # converter that covers many plausible cases.
    # Some are tested below.
    exp <- c(line1 = 1L, col1 = 2L, line2 = 3L, col2 = 4L)

    # <line1>, column <col1> @ <line2>, column <col2>
    expect_identical(
        expected = exp,
        from_tsf_block_loc_range_v1(
            tsf_block_line_token(
                value = "- 1, column 2 @ 3, column 4")))

    # line <line1>, <col1> @ line <line2>, <col2>
    expect_identical(
        expected = exp,
        from_tsf_block_loc_range_v1(
            tsf_block_line_token(
                value = "- line 1, 2 @ line 3, 4")))

    # <line1>,<col1> @ <line2>,<col2>
    expect_identical(
        expected = exp,
        from_tsf_block_loc_range_v1(
            tsf_block_line_token(
                value = "- 1,2 @ 3,4")))

    # <line1>, <col1> @ <line2>, <col2>
    expect_identical(
        expected = exp,
        from_tsf_block_loc_range_v1(
            tsf_block_line_token(
                value = "- 1, 2 @ 3, 4")))
})

test_that("from_tsf_block_loc_range_v1() silently coerces double values", {
    expect_identical(
        expected = c(line1 = 1L, col1 = 2L, line2 = 3L, col2 = 4L),
        from_tsf_block_loc_range_v1(
            tsf_block_line_token(
                value = "- line  1, column 2 @ line 3, column 4.345")))
})


# tokenize_tsf_block_v1() ------------------------------------------------------


test_that("tokenize_tsf_block_v1() returns a list of S3 objects of class BlockLineToken", {
    is_block_line_token <- \(x) vapply_1l(x, inherits, what = "BlockLineToken")

    expect_type(mock_tsf_v1_block_hello_t, "list")
    expect_length(mock_tsf_v1_block_hello_t, length(mock_tsf_v1_block_hello))
    expect_true(all(is_block_line_token(mock_tsf_v1_block_hello_t)))
})

test_that("tokenize_tsf_block_v1() returns an empty list if argument x is empty", {
    expect_identical(tokenize_tsf_block_v1(), list())
})

test_that("tokenize_tsf_block_v1() validates x", {
    expect_error(tokenize_tsf_block_v1(1L))
    expect_snapshot(tokenize_tsf_block_v1(1L), error = TRUE)
})

test_that("tokenize_tsf_block_v1() properly assigns tokens and subtypes", {
    exp <- c(
        # Types and subtypes are encoded
        # in a single vector for brevity.
        # Types       = Subtypes
        TITLE_HASH    = "TITLE_HASH",
        NULL          = "NULL",
        LOC_SRC_PATH  = "LOC_SRC_1",
        LOC_SRC_RNG   = "LOC_SRC_1",
        NULL          = "NULL",
        TITLE_KEY_SRC = "TITLE_KEY_SRC",
        NULL          = "NULL",
        TXT           = "TXT_SRC",
        TXT           = "TXT_SRC",
        TITLE_KEY_TXT = "TITLE_KEY_TXT",
        NULL          = "NULL",
        TXT           = "TXT_TRL_1",
        TXT           = "TXT_TRL_1",
        TITLE_KEY_TXT = "TITLE_KEY_TXT",
        NULL          = "NULL",
        TXT           = "TXT_TRL_2",
        TXT           = "TXT_TRL_2",
        TITLE_KEY_TXT = "TITLE_KEY_TXT",
        NULL          = "NULL",
        TXT           = "TXT_TRL_3")

    exp_types    <- names(exp)
    exp_subtypes <- unname(exp)

    expect_identical(vapply_1c(mock_tsf_v1_block_hello_t, `[[`, i = "type"),    exp_types)
    expect_identical(vapply_1c(mock_tsf_v1_block_hello_t, `[[`, i = "subtype"), exp_subtypes)
})


# tokenize_tsf_block_line_v1() -------------------------------------------------


test_that("tokenize_tsf_block_line_v1() returns a character string", {
    expect_identical(tokenize_tsf_block_line_v1(""),  "TXT")
    expect_identical(tokenize_tsf_block_line_v1("a"), "TXT")
})

test_that("tokenize_tsf_block_line_v1() properly tokenizes strings", {
    expect_identical(tokenize_tsf_block_line_v1("# `{{ hash }}`"),   "TITLE_HASH")
    expect_identical(tokenize_tsf_block_line_v1("## `{{ lang }}`"),  "TITLE_KEY_SRC")
    expect_identical(tokenize_tsf_block_line_v1("## lang"),          "TITLE_KEY_TXT")
    expect_identical(tokenize_tsf_block_line_v1("`path/to/file`:"), "LOC_SRC_PATH")
    expect_identical(tokenize_tsf_block_line_v1("- line 1, column 2 @ line 3, column 4224"),        "LOC_SRC_RNG")
    expect_identical(tokenize_tsf_block_line_v1("-  line  1, column  2  @  line  3, column  4224"), "LOC_SRC_RNG")
    expect_identical(tokenize_tsf_block_line_v1(""),  "TXT")
    expect_identical(tokenize_tsf_block_line_v1("a"), "TXT")

    # Tests below are some edge cases.
    expect_identical(tokenize_tsf_block_line_v1("# {{ hash }}"),   "TXT")
    expect_identical(tokenize_tsf_block_line_v1("## `{ lang }`"),  "TITLE_KEY_TXT")
    expect_identical(tokenize_tsf_block_line_v1("`path/to/file`"), "TXT")
    expect_identical(tokenize_tsf_block_line_v1("path/to/file:"),  "TXT")
    expect_identical(tokenize_tsf_block_line_v1("line  1, column  2  @  line  3, column  4224"), "TXT")
})
