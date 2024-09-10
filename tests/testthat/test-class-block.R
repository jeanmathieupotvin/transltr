hash <- "test-hash"
text <- "Hello, world!"
key  <- "en"
locs <- list(
    location("file1", 1L, 2L, 3L, 4L),
    location("file2", 5L, 6L, 7L, 8L))
trans <- c(
    fr = "Bonjour, monde!",
    es = "¡Hola, mundo!")

test_block <- block(hash, text, key, locs, trans)


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
    block_lines <- format(test_block)

    expect_type(block_lines, "character")
    expect_length(block_lines, 5L)
    expect_identical(block_lines[[1L]], "Hash     : test-hash")
    expect_identical(block_lines[[2L]], "Text     : Hello, world!")
    expect_identical(block_lines[[3L]], "Text key : en")
    expect_identical(block_lines[[4L]], "Lang keys: 'en', 'fr', 'es'")
    expect_identical(block_lines[[5L]], "Locations:\n - file1: ln 1, col 2 @ ln 3, col 4\n - file2: ln 5, col 6 @ ln 7, col 8")
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
