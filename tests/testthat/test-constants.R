# constant() -------------------------------------------------------------------


test_that("constant() returns null for unkown constant names", {
    expect_null(constant("__error__"))
})

test_that("constant() returns expected values", {
    expect_identical(constant("generated-by"), "R package transltr 0.0.1")
    expect_identical(constant("concat"),       " ")
    expect_identical(constant("empty"),        "<none>")
    expect_identical(constant("placeholder"),  "<none>")
    expect_identical(constant("unset"),        "<unset>")
    expect_identical(constant("unknown"),      "<unknown>")
})

test_that("constant() validate which", {
    expect_error(constant(1L))
    expect_snapshot(constant(1L), error = TRUE)
})


# Internal flags ---------------------------------------------------------------


test_that(".__LGL_DEBUG_FLAG is always equal to false", {
    # This is an extra layer of safety to ensure no
    # stupid bugs are forgotten before releasing code.
    expect_false(.__LGL_DEBUG_FLAG)
})
