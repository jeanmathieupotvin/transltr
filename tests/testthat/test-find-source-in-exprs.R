withr::local_options(transltr.verbose = FALSE)

path_mock1 <- get_mock_path(file.path("find-source", "r-script1"))
path_mock2 <- get_mock_path(file.path("find-source", "r-script2"))

tokens_mock1 <- find_source_exprs(path_mock1)
tokens_mock2 <- find_source_exprs(path_mock2)

texts_mock1 <- find_source_in_exprs(tokens_mock1, path_mock1)
texts_mock2 <- find_source_in_exprs(tokens_mock2, path_mock2, interface = quote(translate))

# find_source_in_file() --------------------------------------------------------

test_that("find_source_in_file() works", {
    # This function is just a semantic wrapper for
    # find_source_in_exprs() and therefore, we only
    # check if it returns the output of the former.
    texts_mock1 <- find_source_in_file(path_mock1)
    texts_mock2 <- find_source_in_file(path_mock2, interface = quote(translate))

    expect_type(texts_mock1, "list")
    expect_type(texts_mock2, "list")
    expect_length(texts_mock1, 4L)
    expect_length(texts_mock2, 2L)
    expect_true(all(vapply_1l(texts_mock1, is_text)))
    expect_true(all(vapply_1l(texts_mock2, is_text)))
})

test_that("find_source_in_file() outputs basic information if verbose is true", {
    expect_output(find_source_in_file(path_mock1, verbose = TRUE), "Extracted 4 source text")
})

# find_source_in_exprs() -------------------------------------------------------

test_that("find_source_in_exprs() returns a list of Text objects", {
    expect_type(texts_mock1, "list")
    expect_type(texts_mock2, "list")
    expect_length(texts_mock1, 4L)
    expect_length(texts_mock2, 2L)
    expect_true(all(vapply_1l(texts_mock1, is_text)))
    expect_true(all(vapply_1l(texts_mock2, is_text)))
})

test_that("find_source_in_exprs() processes all calls to $translate() if interface is null", {
    texts_mock1   <- find_source_in_exprs(tokens_mock1, path_mock1)
    texts_mock2   <- find_source_in_exprs(tokens_mock2, path_mock2)
    source_texts1 <- vapply_1c(texts_mock1, `[[`, i = "source_text")
    source_texts2 <- vapply_1c(texts_mock2, `[[`, i = "source_text")

    expect_identical(source_texts1, c(
        "Hello Shiny!",
        "Number of bins:",
        "Waiting time to next eruption (in mins)",
        "Histogram of waiting times"))
    expect_identical(source_texts2, c("e", "f"))
})

test_that("find_source_in_exprs() processes all calls to interface if it not null", {
    texts_mock1   <- find_source_in_exprs(tokens_mock1, path_mock1, interface = quote(translate))
    texts_mock2   <- find_source_in_exprs(tokens_mock2, path_mock2, interface = quote(translate))
    texts_mock3   <- find_source_in_exprs(tokens_mock2, path_mock2, interface = quote(transltr::translate))
    source_texts1 <- vapply_1c(texts_mock1, `[[`, i = "source_text")
    source_texts2 <- vapply_1c(texts_mock2, `[[`, i = "source_text")
    source_texts3 <- vapply_1c(texts_mock3, `[[`, i = "source_text")

    expect_identical(source_texts1, character())
    expect_identical(source_texts2, c("a", "c"))
    expect_identical(source_texts3, c("b", "d"))
})

test_that("find_source_in_exprs() uses source locations returned by the parser", {
    # Locations were determined manually by
    # inspecting mock script rscript-1.
    texts_mock <- find_source_in_exprs(tokens_mock1, path_mock1)
    locations  <- unlist(lapply(texts_mock, `[[`, i = "locations"), FALSE, FALSE)

    expect_identical(locations, list(
        location(path_mock1,  4L, 23L,  4L, 50L),
        location(path_mock1, 12L, 27L, 12L, 59L),
        location(path_mock1, 40L, 22L, 40L, 78L),
        location(path_mock1, 41L, 22L, 41L, 65L)))
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

# is_source() ------------------------------------------------------------------

test_that("is_source() returns a logical", {
    expect_true(is_source(str2lang('tr$translate("test")')))
    expect_false(is_source(str2lang('tr$method("test")')))
})

test_that("is_source() returns false if x is not a call", {
    expect_false(is_source(expression()))
    expect_false(is_source(as.name("symbol")))
})

test_that("is_source() looks for calls to $translate() if interface is null", {
    expect_true(is_source(str2lang('tr$translate("test")')))
    expect_false(is_source(str2lang('translate("test")')))

    # Check that quotes and backticks are handled appropriately.
    expect_true(is_source(str2lang('`tr`$`translate`("test")')))
    expect_true(is_source(str2lang('`$`(tr, translate)("test")')))
    expect_true(is_source(str2lang('`$`(tr, "translate")("test")')))
    expect_true(is_source(str2lang('`$`(`tr`, `translate`)("test")')))
})

test_that("is_source() returns false when calls to $translate() include ...", {
    expect_true(is_source(str2lang('tr$translate()')))
    expect_false(is_source(str2lang('tr$translate(..., lang = language_get())')))
})

test_that("is_source() looks for calls to interface if it is a name", {
    intf <- quote(translate)

    expect_true(is_source(str2lang('translate("test")'),     intf))
    expect_false(is_source(str2lang('tr$translate("test")'), intf))

    # Check that quotes and backticks are handled appropriately.
    expect_true(is_source(str2lang('"translate"("test")'), intf))
    expect_true(is_source(str2lang('`translate`("test")'), intf))
})

test_that("is_source() looks for calls to interface if it is a call", {
    intf <- quote(transltr::translate)

    expect_true(is_source(str2lang('transltr::translate("test")'), intf))
    expect_false(is_source(str2lang('tr$translate("test")'),       intf))

    # Check that quotes and backticks are handled appropriately.
    expect_true(is_source(str2lang('transltr::"translate"("test")'),   intf))
    expect_true(is_source(str2lang('"transltr"::translate("test")'),   intf))
    expect_true(is_source(str2lang('transltr::`translate`("test")'),   intf))
    expect_true(is_source(str2lang('`transltr`::translate("test")'),   intf))
    expect_true(is_source(str2lang('"transltr"::`translate`("test")'), intf))
    expect_true(is_source(str2lang('`transltr`::"translate"("test")'), intf))
})
