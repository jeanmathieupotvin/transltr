mock_tsf_v1_block_hello <- read_text(get_mock_path("tsf-v1-block-hello", "md"))


# tokenize_tsf_block_v1() ------------------------------------------------------


test_that("tokenize_tsf_block_v1() works", {
    skip("Bookmark")
})


# tokenize_tsf_block_line_v1() -------------------------------------------------


test_that("tokenize_tsf_block_line_v1() returns a character string", {
    expect_identical(tokenize_tsf_block_line_v1(""),  "TXT")
    expect_identical(tokenize_tsf_block_line_v1("a"), "TXT")
})

test_that("tokenize_tsf_block_line_v1() properly tokenizes strings", {
    expect_identical(tokenize_tsf_block_line_v1("# `{{ hash }}`"),  "TITLE_HASH")
    expect_identical(tokenize_tsf_block_line_v1("## `{{ key }}`"),  "TITLE_KEY_SRC")
    expect_identical(tokenize_tsf_block_line_v1("## key"),          "TITLE_KEY_TXT")
    expect_identical(tokenize_tsf_block_line_v1("`path/to/file`:"), "LOC_SRC_PATH")
    expect_identical(
        tokenize_tsf_block_line_v1("- line 1, column 2 @ line 3, column 4224"),
        "LOC_SRC_RNG")
    expect_identical(tokenize_tsf_block_line_v1(""),  "TXT")
    expect_identical(tokenize_tsf_block_line_v1("a"), "TXT")

    # Tests below are some edge cases.
    expect_identical(tokenize_tsf_block_line_v1("# {{ hash }}"),   "TXT")
    expect_identical(tokenize_tsf_block_line_v1("## `{ key }`"),   "TITLE_KEY_TXT")
    expect_identical(tokenize_tsf_block_line_v1("`path/to/file`"), "TXT")
    expect_identical(tokenize_tsf_block_line_v1("path/to/file:"),  "TXT")
    expect_identical(
        tokenize_tsf_block_line_v1("-  line  1, column  2  @  line  3, column  4224"),
        "LOC_SRC_RNG")
})
