test_that(".__LGL_DEBUG_FLAG is equal to false", {
    # This is an extra layer of safety to ensure no
    # stupid bugs are forgotten before releasing code.
    expect_false(.__LGL_DEBUG_FLAG)
})
