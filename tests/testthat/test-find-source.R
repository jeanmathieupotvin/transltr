path_mock1    <- get_mock_path(file.path("find-source", "r-script1"))
path_mock2    <- get_mock_path(file.path("find-source", "r-script2"))
path_mock_dir <- dirname(path_mock1)

tokens_mock1 <- find_source_exprs(path_mock1)
tokens_mock2 <- find_source_exprs(path_mock2)

texts_mock1 <- find_source_in_exprs(tokens_mock1, path_mock1)
texts_mock2 <- find_source_in_exprs(tokens_mock2, path_mock2)


# find_source() ----------------------------------------------------------------


test_that("find_source() returns an R6 object of class Translator", {
    out <- find_source(path_mock_dir,
        verbose   = FALSE,
        id        = "test-find-source",
        algorithm = "utf8",
        native_languages = c(
            en = "English",
            fr = "Français",
            el = "Ελληνικά",
            ja = "日本語"))

    expect_s3_class(out, c("Translator", "R6"), exact = TRUE)
    expect_identical(out$id, "test-find-source")
    expect_identical(out$algorithm, "utf8")
    expect_identical(out$hashes, c(
        `950`   = "950",
        `951`   = "951",
        `952`   = "952",
        `10991` = "10991",
        `954`   = "954",
        `955`   = "955",
        `956`   = "956",
        `957`   = "957",
        `958`   = "958",
        `960`   = "960",
        `15878` = "15878",
        `948`   = "948"))
    expect_identical(out$source_texts, c(
        `950`   = "d",
        `951`   = "e",
        `952`   = "f",
        `10991` = "Hello Shiny!",
        `954`   = "h",
        `955`   = "i",
        `956`   = "j",
        `957`   = "k",
        `958`   = "l",
        `960`   = "n",
        `15878` = "Number of bins:",
        `948`   = "b"))
    expect_identical(out$languages, "en")
    expect_identical(out$native_languages, c(
        el = "Ελληνικά",
        en = "English",
        fr = "Français",
        ja = "日本語"))
})

test_that("find_source() validates path", {
    expect_error(find_source(1L))
    expect_error(find_source("non-existent-directory"))
    expect_snapshot(find_source(1L), error = TRUE)
    expect_snapshot(find_source("non-existent-directory"), error = TRUE)
})

test_that("find_source() validates native_languages", {
    expect_error(find_source(native_languages = 1L))
    expect_error(find_source(native_languages = "English"))
    expect_snapshot(find_source(native_languages = 1L),        error = TRUE)
    expect_snapshot(find_source(native_languages = "English"), error = TRUE)
})

test_that("find_source() ignores files not having file extensions R or Rprofile", {
    # Source texts "x" and "y" are in rscript-3, a
    # file with no extension. Therefore, it should
    # be skipped, and x/y should not be registered
    # in the output.
    out <- find_source(path_mock_dir, verbose = FALSE)

    expect_true(all(is.na(match(c("x", "y"), out$source_texts))))
})


# find_source_in_files() -------------------------------------------------------


test_that("find_source_in_files() returns a list of Text objects", {
    paths <- c(path_mock1, path_mock2)
    texts_mode_strict     <- find_source_in_files(paths, verbose = FALSE)
    texts_mode_not_strict <- find_source_in_files(paths, strict = FALSE, verbose = FALSE)

    expect_type(texts_mode_strict,     "list")
    expect_type(texts_mode_not_strict, "list")
    expect_length(texts_mode_strict,     12L)
    expect_length(texts_mode_not_strict, 18L)
    expect_true(all(vapply_1l(texts_mode_strict, is_text)))
    expect_true(all(vapply_1l(texts_mode_not_strict, is_text)))
})

test_that("find_source_in_files() validates paths", {
    expect_error(find_source_in_files(1L))
    expect_snapshot(find_source_in_files(1L), error = TRUE)
})

test_that("find_source_in_files() validates strict", {
    expect_error(find_source_in_files(path_mock1, strict = 1L))
    expect_snapshot(find_source_in_files(path_mock1, strict = 1L), error = TRUE)
})

test_that("find_source_in_files() validates algorithm", {
    expect_error(find_source_in_files(path_mock1, algorithm = 1L))
    expect_snapshot(find_source_in_files(path_mock1, algorithm = 1L), error = TRUE)
})

test_that("find_source_in_files() validates verbose", {
    expect_error(find_source_in_files(path_mock1, verbose = 1L))
    expect_snapshot(find_source_in_files(path_mock1, verbose = 1L), error = TRUE)
})
