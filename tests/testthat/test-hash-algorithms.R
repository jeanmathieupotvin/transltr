test_that("hash_algorithms() returns expected values", {
    expect_identical(hash_algorithms(), c("sha1", "utf8"))
    expect_snapshot(hash_algorithms())
})
