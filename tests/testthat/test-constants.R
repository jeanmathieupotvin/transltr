test_that("it returns null for unkown constant names", {
    expect_null(constant("__error__"))
})

test_that("it returns expected values", {
    expect_identical(constant("algorithms"),  c("sha1", "utf8"))
    expect_identical(constant("concat"),  " ")
    expect_identical(constant("null"),    "<null>")
    expect_identical(constant("empty"),   "<empty>")
    expect_identical(constant("unset"),   "<unset>")
    expect_identical(constant("unknown"), "<unknown>")
    expect_identical(
        constant("untranslated"),
        "# Erase this comment and provide a translation.")
})

test_that("it validate which", {
    expect_error(constant(1L))
    expect_snapshot(constant(1L), error = TRUE)
})

test_that(".__LGL_DEBUG_FLAG is equal to false", {
    # This is an extra layer of safety to ensure no
    # stupid bugs are forgotten before releasing code.
    expect_false(.__LGL_DEBUG_FLAG)
})
