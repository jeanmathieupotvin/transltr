test_that("trickRoxyygenTemporarily() throws an error until it is replaced", {
    # FIXME: replace me by next misc function.
    expect_error(trickRoxyygenTemporarily())
    expect_snapshot_error(trickRoxyygenTemporarily())
})

test_that("%||% works", {
    expect_identical(NULL %||% 2L, 2L)
    expect_identical(1L   %||% 2L, 1L)
})
