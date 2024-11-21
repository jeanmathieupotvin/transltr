path_mock1    <- get_mock_path("scripts/find-source-1")
path_mock2    <- get_mock_path("scripts/find-source-2")
path_mock_dir <- dirname(path_mock1)
tokens_mock1  <- find_source_exprs(path_mock1)
tokens_mock2  <- find_source_exprs(path_mock2)
texts_mock1   <- find_source_in_exprs(tokens_mock1, path_mock1)
texts_mock2   <- find_source_in_exprs(tokens_mock2, path_mock2)


# find_source() ----------------------------------------------------------------


test_that("find_source() returns a Translator object", {
    out <- find_source(path_mock_dir,
        id               = "test-find-source",
        hash_algorithm   = "utf8",
        native_languages = c(
            en = "English",
            fr = "Français",
            el = "Ελληνικά",
            ja = "日本語"))

    expect_s3_class(out, c("Translator", "R6"), exact = TRUE)
    expect_identical(out$id, "test-find-source")
    expect_identical(out$hash_algorithm, "utf8")
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
    # Source texts "x" and "y" are in scripts/find-source-3, a
    # file with no extension. Therefore, it should be skipped,
    # and x/y should not be registered in the output.
    out <- find_source(path_mock_dir)
    expect_true(all(is.na(match(c("x", "y"), out$source_texts))))
})


# find_source_in_files() -------------------------------------------------------


test_that("find_source_in_files() returns a list of Text objects", {
    paths <- c(path_mock1, path_mock2)
    texts_mode_strict     <- find_source_in_files(paths)
    texts_mode_not_strict <- find_source_in_files(paths, strict = FALSE)

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

test_that("find_source_in_files() validates hash_algorithm", {
    expect_error(find_source_in_files(path_mock1, hash_algorithm = 1L))
    expect_snapshot(find_source_in_files(path_mock1, hash_algorithm = 1L), error = TRUE)
})

test_that("find_source_in_files() validates verbose", {
    expect_error(find_source_in_files(path_mock1, verbose = 1L))
    expect_snapshot(find_source_in_files(path_mock1, verbose = 1L), error = TRUE)
})


# find_source_in_file() --------------------------------------------------------


test_that("find_source_in_file() works", {
    # This function is just a semantic wrapper for
    # find_source_in_exprs() and therefore, we only
    # check if it returns the output of the former.
    texts_mock1 <- find_source_in_file(path_mock1)
    texts_mock2 <- find_source_in_file(path_mock2)

    expect_type(texts_mock1, "list")
    expect_type(texts_mock2, "list")
    expect_length(texts_mock1, 2L)
    expect_length(texts_mock2, 10L)
    expect_true(all(vapply_1l(texts_mock1, is_text)))
    expect_true(all(vapply_1l(texts_mock2, is_text)))
})

test_that("find_source_in_file() outputs basic information if .verbose is true", {
    expect_output(find_source_in_file(path_mock1, .verbose = TRUE), "Extracted 2 source text")
    expect_output(find_source_in_file(path_mock2, .verbose = TRUE), "Extracted 10 source text")
})


# find_source_in_exprs() -------------------------------------------------------


test_that("find_source_in_exprs() returns a list of Text objects", {
    expect_type(texts_mock1, "list")
    expect_type(texts_mock2, "list")
    expect_length(texts_mock1, 2L)
    expect_length(texts_mock2, 10L)
    expect_true(all(vapply_1l(texts_mock1, is_text)))
    expect_true(all(vapply_1l(texts_mock2, is_text)))
})

test_that("find_source_in_exprs() only processes explicit calls if .strict is true", {
    # See the documentation of is_translate_call() for
    # more information on implicit and explicit calls.
    source_texts1 <- vapply_1c(texts_mock1, `[[`, i = "source_text")
    source_texts2 <- vapply_1c(texts_mock2, `[[`, i = "source_text")

    expect_identical(source_texts1, c("Hello Shiny!", "Number of bins:"))
    expect_identical(source_texts2, c("b", "d", "e", "f", "h", "i", "j", "k", "l", "n"))
})

test_that("find_source_in_exprs() processes all calls if .strict is false", {
    texts_mock1  <- find_source_in_exprs(tokens_mock1, path_mock1, FALSE)
    texts_mock2  <- find_source_in_exprs(tokens_mock2, path_mock2, FALSE)
    source_texts1 <- vapply_1c(texts_mock1, `[[`, i = "source_text")
    source_texts2 <- vapply_1c(texts_mock2, `[[`, i = "source_text")

    expect_identical(source_texts1, c(
        "Hello Shiny!",
        "Number of bins:",
        "Waiting time to next eruption (in mins)",
        "Histogram of waiting times"))
    expect_identical(source_texts2, c(
        "a", "b", "c", "d", "e",
        "f", "g", "h", "i", "j",
        "k", "l", "m", "n"))
})

test_that("find_source_in_exprs() uses source locations returned by the parser", {
    # Locations were determined manually by
    # inspecting mock script find-source-1.
    texts_mock <- find_source_in_exprs(tokens_mock1, path_mock1, FALSE)
    locations   <- unlist(lapply(texts_mock, `[[`, i = "locations"), FALSE, FALSE)

    expect_identical(locations, list(
        location(path_mock1, 21L, 23L, 21L, 57L),
        location(path_mock1, 29L, 27L, 29L, 64L),
        location(path_mock1, 61L, 22L, 61L, 73L),
        location(path_mock1, 62L, 22L, 62L, 60L)))
})


# find_source_exprs() ----------------------------------------------------------


test_that("find_source_exprs() returns a data.frame of expr tokens", {
    required_fields <- c("line1", "col1", "line2", "col2", "text")

    expect_s3_class(tokens_mock1, "data.frame")
    expect_s3_class(tokens_mock2, "data.frame")
    expect_contains(names(tokens_mock1), required_fields)
    expect_contains(names(tokens_mock2), required_fields)
    expect_true(all(tokens_mock1$token == "expr"))
    expect_true(all(tokens_mock2$token == "expr"))
})
