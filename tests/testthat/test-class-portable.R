# Degenerate as_*() methods ----------------------------------------------------


test_that("as_location.Location() works", {
    loc <- location()
    expect_identical(as_location(loc), loc)
})

test_that("as_text.Text() works", {
    txt <- text(en = "Hello, world!")
    expect_identical(as_text(txt), txt)
})
