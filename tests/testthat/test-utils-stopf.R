test_that("stopf() throws errors", {
    expect_error(stopf("error"))
    expect_snapshot_error(
        stopf(
            "this is a %s", "error", "%s.",
            .args = list("useful", "message")))
})

test_that("stopf() concatenates error messages", {
    expect_snapshot_error(stopf("message 1 and", "message 2"))
})

test_that("stopf() substitutes conversion specifications", {
    expect_snapshot_error(
        stopf(
            "the %s was substituted.",
            .args = list("conversion specification")))
})
