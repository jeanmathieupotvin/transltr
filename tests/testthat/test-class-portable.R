# portable() -------------------------------------------------------------------


test_that("portable() returns x", {
    expect_identical(portable(1L), 1L, ignore_attr = TRUE)
})

test_that("portable() returns a Portable object", {
    expect_s3_class(portable(1L), "Portable")
})

test_that("portable() validates super", {
    expect_error(portable(1L, super = 1L))
    expect_snapshot(portable(1L, super = 1L), error = TRUE)
})

test_that("portable() validates tag if not null", {
    expect_error(portable(1L, tag = 1L))
    expect_snapshot(portable(1L, tag = 1L), error = TRUE)
})

test_that("portable() adds a tag attribute if not null", {
    expect_identical(attr(portable(1L, tag = "Integer"), "tag"), "Integer")
})

test_that("portable() adds super-classes if any", {
    super_classes <- c("SuperPortableInteger", "PortableInteger")
    expect_s3_class(portable(1L, super_classes), super_classes)
})


# is_portable() ----------------------------------------------------------------


test_that("is_portable() returns a logical", {
    expect_true(is_portable(portable(1L)))
    expect_false(is_portable(1L))
})


# portable_location() ----------------------------------------------------------


test_that("portable_location() returns an S3 object of class PortableLocation", {
    loc <- location("test-file", 1L, 2L, 3L, 4L)
    out <- portable_location(loc)

    expect_s3_class(out, "PortableLocation")
    expect_identical(attr(out, "tag"), "Location")
    expect_identical(out$path, out$path)
    expect_identical(out$ranges, "line 1, column 2 @ line 3, column 4")
})

test_that("portable_location() validates x", {
    expect_error(portable_location(1L))
    expect_snapshot(portable_location(1L), error = TRUE)
})


# Degenerate as_*() methods ----------------------------------------------------


test_that("as_location.Location() works", {
    loc <- location()
    expect_identical(as_location(loc), loc)
})

test_that("as_text.Text() works", {
    txt <- text(en = "Hello, world!")
    expect_identical(as_text(txt), txt)
})
