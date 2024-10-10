test_block <- function() {
    return(
        block("en",
            location("a", 1L, 2L, 3L, 4L),
            location("a", 1L, 2L, 3L, 4L),
            location("b", 5L, 6L, 7L, 8L),
            en = "Hello, world!",
            fr = "Bonjour, monde!",
            es = "¡Hola Mundo!",
            jp = "こんにちは世界！"))
}

blk <- test_block()


# Class: active bindings -------------------------------------------------------


test_that("active binding hash returns registered hash", {
    expect_identical(blk$hash, "b5e480d5ff9fa8583c5caa4c7b63f0719cc878e8")
})

test_that("active binding hash throws an error if value is not missing", {
    expect_error(blk$hash <- "new-hash")
    expect_snapshot(blk$hash <- "new-hash", error = TRUE)
})

test_that("active binding hash_algorithm returns registered hash_algorithm", {
    expect_identical(blk$hash_algorithm, "sha1")
})

test_that("active binding hash_algorithm validates value", {
    expect_error(blk$hash_algorithm <- 1L)
    expect_error(blk$hash_algorithm <- "new-algo")
    expect_snapshot(blk$hash_algorithm <- "new-algo", error = TRUE)
})

test_that("active binding hash_algorithm sets new value and new hash", {
    blk <- test_block()
    blk$hash_algorithm <- "utf8"
    expect_identical(blk$hash_algorithm, "utf8")
    expect_identical(blk$hash, "12351")
})

test_that("active binding source_key returns registered source_key", {
    expect_identical(blk$source_key, "en")
})

test_that("active binding source_key validates value", {
    expect_error(blk$source_key <- 1L)
    expect_error(blk$source_key <- "new-key")
    expect_snapshot(blk$source_key <- "new-key", error = TRUE)
})

test_that("active binding source_key sets new value and new hash", {
    blk <- test_block()
    blk$source_key <- "fr"
    expect_identical(blk$source_key, "fr")
    expect_identical(blk$hash, "4755d5a2e4dc0d7a8d599655e5d0c22d51db752d")
})

test_that("active binding source_text returns registered source_text", {
    expect_identical(blk$source_text, "Hello, world!")
})

test_that("active binding source_text throws an error if value is not missing", {
    expect_error(blk$source_text <- "new-text")
    expect_snapshot(blk$source_text <- "new-text", error = TRUE)
})

test_that("active binding keys returns registered language keys", {
    # This implicitly checks that keys are also sorted.
    keys <- blk$keys
    expect_identical(keys, c("en", "es", "fr", "jp"), ignore_attr = TRUE)
    expect_identical(attr(keys, "source_key"), "en")
})

test_that("active binding keys throws an error if value is not missing", {
    expect_error(blk$keys <- "new-key")
    expect_snapshot(blk$keys <- "new-key", error = TRUE)
})

test_that("active binding translations returns registered translations", {
    # This implicitly checks that translations are
    # also sorted by their underlying names (keys).
    expect_identical(blk$translations, c(
        en = "Hello, world!",
        es = "¡Hola Mundo!",
        fr = "Bonjour, monde!",
        jp = "こんにちは世界！"))
})

test_that("active binding translations throws an error if value is not missing", {
    expect_error(blk$translations <- "new translation")
    expect_snapshot(blk$translations <- "new-translation", error = TRUE)
})

test_that("active binding locations returns registered locations", {
    # This implicitly checks that translations are
    # also sorted by their underlying names (keys).
    expect_identical(blk$locations, list(
        location("a", 1L, 2L, 3L, 4L),
        location("b", 5L, 6L, 7L, 8L)))
})

test_that("active binding locations throws an error if value is not missing", {
    expect_error(blk$locations <- location())
    expect_snapshot(blk$locations <- location(), error = TRUE)
})


# Class: private methods -------------------------------------------------------


test_that("$.hash_do() returns a character string", {
    blk  <- test_block()
    sha1 <- blk$.__enclos_env__$private$.hash_do("en", "Hello, world!")

    blk$hash_algorithm <- "utf8"
    utf8 <- blk$.__enclos_env__$private$.hash_do("en", "Hello, world!")

    expect_identical(sha1, "b5e480d5ff9fa8583c5caa4c7b63f0719cc878e8")
    expect_identical(utf8, "12351")
})


# Class: public methods --------------------------------------------------------


test_that("$initialize() works", {
    # It can only be tested indirectly via $new().
    blk <- Block$new("utf8")
    expect_identical(blk$hash_algorithm, "utf8")
    expect_type(blk$.__enclos_env__$private$.translations, "environment")
})

test_that("$initialize() validates hash_algorithm", {
    expect_error(Block$new("error"))
    expect_snapshot(Block$new("error"), error = TRUE)
})

test_that("$get_translation() works", {
    expect_null(blk$get_translation("error"))
    expect_identical(blk$get_translation("en"), "Hello, world!")
    expect_identical(blk$get_translation("es"), "¡Hola Mundo!")
    expect_identical(blk$get_translation("fr"), "Bonjour, monde!")
    expect_identical(blk$get_translation("jp"), "こんにちは世界！")
})

test_that("$get_translation() validates key", {
    expect_error(blk$get_translation(1L))
    expect_snapshot(blk$get_translation(1L), error = TRUE)
})

test_that("$set_translation() works", {
    blk <- Block$new()
    expect_true(blk$set_translation("en", "Hello, world!"))
    expect_invisible(blk$set_translation("en", "Hello, world!"))
    expect_identical(blk$get_translation("en"), "Hello, world!")
})

test_that("$set_translation() validates key", {
    expect_error(blk$set_translation(1L))
    expect_snapshot(blk$set_translation(1L), error = TRUE)
})

test_that("$set_translation() validates text", {
    expect_error(blk$set_translation("de", 1L))
    expect_snapshot(blk$set_translation("de", 1L), error = TRUE)
})

test_that("$set_translations() works", {
    blk <- Block$new()

    # Case ... is empty.
    expect_true(Block$new()$set_translations())
    expect_invisible(Block$new()$set_translations())

    # Case ... is not empty.
    expect_true(blk$set_translations(en = "Hello, world!"))
    expect_invisible(blk$set_translations(fr = "Bonjour, monde!"))
    expect_identical(blk$get_translation("en"), "Hello, world!")
    expect_identical(blk$get_translation("fr"), "Bonjour, monde!")
})

test_that("$set_translations() validates ...", {
    blk <- Block$new()
    expect_error(blk$set_translations(1L))
    expect_error(blk$set_translations("Hello, world!"))
    expect_snapshot(blk$set_translations(1L),              error = TRUE)
    expect_snapshot(blk$set_translations("Hello, world!"), error = TRUE)
})

test_that("$set_locations() returns a logical", {
    blk <- Block$new()

    # Case ... is empty.
    expect_true(Block$new()$set_locations())
    expect_invisible(Block$new()$set_locations())

    # Case ... is not empty.
    expect_true(blk$set_locations(location("z", 1L, 1L, 1L, 1L)))
    expect_invisible(blk$set_locations(location("z", 2L, 2L, 2L, 2L)))
    expect_length(blk$locations, 1L)
    expect_identical(
        blk$locations[[1L]],
        location("z", c(1L, 2L), c(1L, 2L), c(1L, 2L), c(1L, 2L)))
})

test_that("$rm_translation() returns a logical", {
    expect_true(test_block()$rm_translation("es"))
    expect_invisible(test_block()$rm_translation("fr"))
})

test_that("$rm_translation() validates key", {
    expect_error(blk$rm_translation(1L))
    expect_error(blk$rm_translation("en"))
    expect_error(blk$rm_translation("error"))
    expect_snapshot(blk$rm_translation(1L),      error = TRUE)
    expect_snapshot(blk$rm_translation("en"),    error = TRUE)
    expect_snapshot(blk$rm_translation("error"), error = TRUE)
})

test_that("$rm_translation() removes translations as expected", {
    blk <- test_block()
    blk$rm_translation("es")
    expect_length(blk$translations, 3L)
    expect_identical(blk$translations, c(
        en = "Hello, world!",
        fr = "Bonjour, monde!",
        jp = "こんにちは世界！"))
})

test_that("$rm_location() returns a logical", {
    expect_true(test_block()$rm_location("a"))
    expect_invisible(test_block()$rm_location("a"))
})

test_that("$rm_location() validates path", {
    expect_error(blk$rm_location(1L))
    expect_error(blk$rm_location("error"))
    expect_snapshot(blk$rm_location(1L),      error = TRUE)
    expect_snapshot(blk$rm_location("error"), error = TRUE)
})

test_that("$rm_location() removes locations as expected", {
    blk <- test_block()
    blk$rm_location("a")
    expect_length(blk$locations, 1L)
    expect_identical(blk$locations, list(location("b", 5L, 6L, 7L, 8L)))
})


# Constructors -----------------------------------------------------------------


# TODO: here.
