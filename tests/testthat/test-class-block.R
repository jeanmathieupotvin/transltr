test_block <- block(
    hash <- "test-hash",
    text <- "Hello, world!",
    key  <- "en",
    locs <- list(
        location("file1", 1L, 2L, 3L, 4L),
        location("file2", c(1L, 2L), c(3L, 4L), c(5L, 6L), c(7L, 8L))),
    trans <- c(
        fr = "Bonjour, monde!",
        es = "¡Hola, mundo!"))

fmt_block <- format(test_block)


# block() ----------------------------------------------------------------------


test_that("block() returns a S3 object of class Block", {
    expect_s3_class(test_block, "Block")
    expect_type(test_block, "list")
    expect_length(test_block, 6L)
    expect_identical(test_block$hash, hash)
    expect_identical(test_block$text, text)
    expect_identical(test_block$text_key, key)
    expect_identical(test_block$locations, locs)
    expect_identical(test_block$translations, trans)
    expect_identical(test_block$.texts, c(text, trans), ignore_attr = "names")
    expect_named(test_block$.texts, c(key, names(trans)))
})

test_that("block() validates argument hash", {
    expect_error(block(1L))
    expect_snapshot(block(1L), error = TRUE)
})

test_that("block() validates argument text", {
    expect_error(block(hash, text = 1L))
    expect_snapshot(block(hash, text = 1L), error = TRUE)
})

test_that("block() validates argument text_key", {
    expect_error(block(hash, text, text_key = 1L))
    expect_snapshot(block(hash, text, text_key = 1L), error = TRUE)
})

test_that("block() validates argument locations", {
    expect_error(block(hash, text, key, locations = 1L))
    expect_error(block(hash, text, key, locations = list(1L)))
    expect_snapshot(block(hash, text, key, locations = 1L),       error = TRUE)
    expect_snapshot(block(hash, text, key, locations = list(1L)), error = TRUE)
})

test_that("block() validates argument translations", {
    expect_error(block(hash, text, key, locs, 1L))
    expect_error(block(hash, text, key, locs, unname(trans)))
    expect_snapshot(block(hash, text, key, locs, 1L),            error = TRUE)
    expect_snapshot(block(hash, text, key, locs, unname(trans)), error = TRUE)
})

test_that("block() silently coerces argument translations to a vector", {
    expect_no_error(block(hash, text, key, locs, as.list(trans)))
})


# is_block() -------------------------------------------------------------------


test_that("is_block() works", {
    expect_true(is_block(test_block))
    expect_false(is_block(1L))
})


# format.Block() ---------------------------------------------------------------


test_that("format() returns a character string", {
    # This test block is a little bit
    # fragile, but hardcoding expected
    # values is much more simpler.

    expect_type(fmt_block, "character")
    expect_identical(fmt_block[[1L]], "<Block>")
    expect_identical(fmt_block[[2L]], "  Hash     : test-hash")
    expect_identical(fmt_block[[3L]], "  Text     : Hello, world!")
    expect_identical(fmt_block[[4L]], "  Text key : en")
    expect_identical(fmt_block[[5L]], "  Lang keys: 'en', 'fr', 'es'")
})

test_that("format() truncates text if it cannot fit on a single line", {
    long_text <- paste(
        "This is a very long and very boring character string written in",
        "such a way that it contains more than 64 individual characters",
        "and therefore cannot fit on a single line. This will trigger the",
        "underlying truncation logic of format.Block().",
        sep = " ")

    block <- block(hash, long_text, key, locs, trans)
    expect_match(format(block)[[3L]], "\\.\\.\\.$")
    expect_identical(nchar(format(block)[[3L]]), 80L)
})

test_that("format() includes formatted Location objects in the output", {
    # We remove the extra padding to ease the comparison.
    # We want to check all format.Location() outputs are
    # included in the output.
    fmt_locs       <- lapply(test_block$locations, format)
    fmt_locs_block <- gsub("^ {2}", "", fmt_block[6L:12L])

    expect_identical(fmt_locs_block[1L:3L], fmt_locs[[1L]])
    expect_identical(fmt_locs_block[4L:7L], fmt_locs[[2L]])
})


# print.Block() ----------------------------------------------------------------


test_that("print() returns its argument invisibly", {
    withr::local_output_sink(tempfile())
    expect_invisible(print(test_block))
    expect_identical(print(test_block), test_block)
})

test_that("print() works", {
    expect_output(print(test_block), "^<Block>")
    expect_snapshot(print(test_block))
})
