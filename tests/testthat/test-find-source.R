withr::local_options(transltr.verbose = FALSE)

path_mock1    <- get_mock_path(file.path("find-source", "r-script1"))
path_mock2    <- get_mock_path(file.path("find-source", "r-script2"))
path_mock_dir <- dirname(path_mock1)

tokens_mock1 <- find_source_exprs(path_mock1)
tokens_mock2 <- find_source_exprs(path_mock2)

texts_mock1 <- find_source_in_exprs(tokens_mock1, path_mock1)
texts_mock2 <- find_source_in_exprs(tokens_mock2, path_mock2)

# find_source() ----------------------------------------------------------------

test_that("find_source() returns an R6 object of class Translator", {
    tr  <- translator(id = "test-find-source", algorithm = "utf8")
    out <- find_source(path_mock_dir, tr = tr)

    expect_s3_class(out, c("Translator", "R6"), exact = TRUE)
    expect_identical(out$hashes, c(
        `951`   = "951",
        `952`   = "952",
        `10991` = "10991",
        `41896` = "41896",
        `15878` = "15878",
        `85295` = "85295"))
    expect_identical(out$source_texts, c(
        `951`   = "e",
        `952`   = "f",
        `10991` = "Hello Shiny!",
        `41896` = "Histogram of waiting times",
        `15878` = "Number of bins:",
        `85295` = "Waiting time to next eruption (in mins)"))
    expect_identical(out$languages, "en")
})

test_that("find_source() validates path", {
    expect_error(find_source(1L))
    expect_error(find_source("non-existent-directory"))
    expect_snapshot(find_source(1L), error = TRUE)
    expect_snapshot(find_source("non-existent-directory"), error = TRUE)
})

test_that("find_source() validates tr", {
    expect_error(find_source(tr = 1L))
    expect_snapshot(find_source(tr = 1L), error = TRUE)
})

test_that("find_source() ignores files not having file extensions R or Rprofile", {
    # Source texts "x" and "y" are in rscript-3, a
    # file with no extension. Therefore, it should
    # be skipped, and x/y should not be registered
    # in the output.
    out <- find_source(path_mock_dir)

    expect_true(all(is.na(match(c("x", "y"), out$source_texts))))
})

# find_source_in_files() -------------------------------------------------------

test_that("find_source_in_files() returns a list of Text objects", {
    paths <- c(path_mock1, path_mock2)
    texts_method    <- find_source_in_files(paths)
    texts_interface <- find_source_in_files(paths, interface = quote(translate))

    expect_type(texts_method,    "list")
    expect_type(texts_interface, "list")
    expect_length(texts_method,    6L)
    expect_length(texts_interface, 2L)
    expect_true(all(vapply_1l(texts_method, is_text)))
    expect_true(all(vapply_1l(texts_interface, is_text)))
})

test_that("find_source_in_files() validates paths", {
    expect_error(find_source_in_files(1L))
    expect_snapshot(find_source_in_files(1L), error = TRUE)
})

test_that("find_source_in_files() validates verbose", {
    expect_error(find_source_in_files(path_mock1, verbose = 1L))
    expect_snapshot(find_source_in_files(path_mock1, verbose = 1L), error = TRUE)
})

test_that("find_source_in_files() validates algorithm", {
    expect_error(find_source_in_files(path_mock1, algorithm = 1L))
    expect_snapshot(find_source_in_files(path_mock1, algorithm = 1L), error = TRUE)
})

test_that("find_source_in_files() validates interface", {
    expect_no_error(find_source_in_files(path_mock1, interface = as.name("foo")))
    expect_no_error(find_source_in_files(path_mock1, interface = call("::", "pkg", "foo")))
    expect_no_error(find_source_in_files(path_mock1, interface = quote(foo)))
    expect_no_error(find_source_in_files(path_mock1, interface = quote(pkg::foo)))

    expect_error(find_source_in_files(path_mock1, interface = 1L))
    expect_error(find_source_in_files(path_mock1, interface = call("foo")))
    expect_snapshot(find_source_in_files(path_mock1, interface = 1L), error = TRUE)
})
