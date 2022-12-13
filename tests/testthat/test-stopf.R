test_that("stopf() throws error", {
    expect_error(stopf("TypeError", "argument must contain %i values.", 7L))
})

test_that("stopf() validates type", {
    expect_error(stopf("ErrorInjected"))
    expect_snapshot_error(stopf("ErrorInjected"))
})
