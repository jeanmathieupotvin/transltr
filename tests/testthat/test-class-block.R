test_hash  <- "test-hash"
test_trans <- c(fr = "fr", en = "en")
test_block <- new_block(test_hash, test_trans)


# new_block() ------------------------------------------------------------------


test_that("new_block() returns a Block object", {
    expect_s3_class(test_block, c("Block", "list"))
    expect_identical(test_block$hash, test_hash)
    expect_identical(test_block$translations, test_trans)
})

test_that("new_block() validates argument hash", {
    expect_error(new_block(1L))

    expect_snapshot(new_block(1L), error = TRUE)
})

test_that("new_block() validates argument translations", {
    expect_error(new_block(test_hash, 1L))
    expect_error(new_block(test_hash, c("fr", "en")))

    expect_snapshot(new_block(test_hash, 1L),            error = TRUE)
    expect_snapshot(new_block(test_hash, c("fr", "en")), error = TRUE)
})


# is_block() -------------------------------------------------------------------


test_that("is_block() works", {
    expect_true(is_block(test_block))
    expect_false(is_block(1L))
})
