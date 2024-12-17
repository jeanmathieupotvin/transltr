# stops() ----------------------------------------------------------------------


test_that("stops() does not return the call as part of the error message", {
    expect_error(stops())
    expect_snapshot(error = TRUE, {
        wrap_stops <- \() stops("this is an error message.")
        wrap_stops()
    })
})


# stopf() ----------------------------------------------------------------------


test_that("stopf() works", {
    expect_error(stopf())
    expect_snapshot(
        stopf("this '%s' becomes part of the error message.", "placeholder"),
        error = TRUE)
})
