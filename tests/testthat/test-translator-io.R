language_source_set("en")
withr::defer(language_source_set(NULL))

tr <- translator(
    id = "test-translator",
    el = "Ελληνικά",
    en = "English",
    es = "Español",
    fr = "Français",
    text(
        location("a", 1L, 2L, 3L, 4L),
        en = "Hello, world!",
        es = "¡Hola Mundo!",
        fr = "Bonjour, monde!"),
    text(
        location("b", 5L, 6L, 7L, 8L),
        en = "Farewell, world!",
        fr = "Au revoir, monde!"))

# Comments below are copied and pasted from the source script.
# This is a little bit fragile, but hardcoding expected values is simpler.
comments_translator_file <- c(
    "# Translator",
    "#",
    "# - You may edit fields Identifier, and Languages.",
    "# - Do not edit other fields by hand. Edit source scripts instead.",
    "%YAML 1.1")

comments_translations_file <- c(
    "# Translations",
    "#",
    "# - Edit each 'Translation' section below.",
    "# - Choose UTF-8 whenever you have to select a character encoding.",
    "# - You may use any text editor.",
    "# - You may split long sentences with single new lines.",
    "# - You may include comments.",
    "#   - What follows an octothorpe (#) is ignored until the next line.",
    "#   - An escaped octothorpe (\\#) is treated as normal text.")


# translations_paths() ---------------------------------------------------------


test_that("translations_paths() returns a named character", {
    # translations_paths() exclude the source language
    # because it does not require a translations file.
    out <- translations_paths(tr)

    expect_type(out, "character")
    expect_length(out, length(tr$native_languages) - 1L)
    expect_named(out)
})

test_that("translations_paths() validates tr", {
    tr <- translator(
        en = "English",
        fr = "Français",
        text(
            location("a", 1L, 2L, 3L, 4L),
            en = "Hello, world!",
            fr = "Bonjour, monde!",
            source_lang = "en"),
        text(
            location("b", 5L, 6L, 7L, 8L),
            en = "Farewell, world!",
            fr = "Au revoir, monde!",
            source_lang = "fr"))

    expect_error(translations_paths(1L))
    expect_error(translations_paths(tr))
    expect_snapshot(translations_paths(1L), error = TRUE)
    expect_snapshot(translations_paths(tr), error = TRUE)
})

test_that("translations_paths() validates parent_dir", {
    expect_error(translations_paths(tr, 1L))
    expect_snapshot(translations_paths(tr, 1L), error = TRUE)
})

test_that("translations_paths() does not include source language", {
    out <- translations_paths(tr)

    expect_named(out, c("el", "es", "fr"))
    expect_identical(out["en"], NA_character_, ignore_attr = TRUE)
})

test_that("translations_paths() returns expected file paths", {
    # normalizePath() is required on Windows because
    # dirname(), and file.path() respectively uses \\
    # and / as the file separator. This inconsistency
    # must be fixed for testing purposes.
    temp_file <- normalizePath(
        file.path(tempdir(), "_translator.yml"),
        winslash = "/",
        mustWork = FALSE)

    withr::local_options(transltr.default.path = temp_file)

    expect_identical(
        translations_paths(tr),
        structure(
            normalizePath(
                file.path(tempdir(), c("el.txt", "es.txt", "fr.txt")),
                winslash = "/",
                mustWork = FALSE),
            names = c("el", "es", "fr")))
})


# translator_write() -----------------------------------------------------------


test_that("translator_write() returns null invisibly", {
    temp_file <- withr::local_tempfile(pattern = "_translator_", fileext = ".yml")
    withr::defer(file.remove(translations_paths(tr, dirname(temp_file))))

    expect_null(translator_write(tr, temp_file, TRUE, FALSE))
    expect_invisible(translator_write(tr, temp_file, TRUE, FALSE))
})

test_that("translator_write() validates path", {
    expect_error(translator_write(tr, 1L))
    expect_snapshot(translator_write(tr, 1L), error = TRUE)
})

test_that("translator_write() validates overwrite", {
    expect_error(translator_write(tr, overwrite = 1L))
    expect_snapshot(translator_write(tr, overwrite = 1L), error = TRUE)
})

test_that("translator_write() validates verbose", {
    expect_error(translator_write(tr, verbose = 1L))
    expect_snapshot(translator_write(tr, verbose = 1L), error = TRUE)
})

test_that("translator_write() validates translations", {
    expect_error(translator_write(tr, translations = 1L))
    expect_snapshot(translator_write(tr, translations = 1L), error = TRUE)
})

test_that("translator_write() throws an error if path exists and overwrite is false", {
    temp_file <- withr::local_tempfile(pattern = "_translator_", fileext = ".yml")
    withr::defer(file.remove(translations_paths(tr, dirname(temp_file))))
    translator_write(tr, temp_file, verbose = FALSE)

    expect_error(translator_write(tr, temp_file, FALSE))
    expect_snapshot(translator_write(tr, temp_file, FALSE), error = TRUE)
})

test_that("translator_write() throws an error if parent directories cannot be created", {
    # Sys.chmod() is incompatible with Windows,
    # but is required for testing purposes. See
    # doc for more information.
    skip_on_os("windows")

    # Create a directory with permissions 444.
    # 444 = read-only for owner / group / other.
    # It can still be deleted afterwards.
    temp_dir <- withr::local_tempdir("test-dir")
    Sys.chmod(temp_dir, mode = "444")

    # Set path to Translator file in a subdirectory of
    # temp_dir that must be created by translator_write().
    # This is (obviously) not possible (mode is 444).
    temp_file <- file.path(temp_dir, "transltr", "_translator.yml")

    expect_error(translator_write(tr, temp_file))
    expect_snapshot(translator_write(tr, temp_file), error = TRUE)
})

test_that("translator_write() includes comments", {
    temp_file <- withr::local_tempfile(pattern = "_translator_", fileext = ".yml")
    withr::defer(file.remove(translations_paths(tr, dirname(temp_file))))
    translator_write(tr, temp_file, verbose = FALSE)
    text <- text_read(temp_file)

    expect_identical(
        text[seq_along(comments_translator_file)],
        comments_translator_file)
})

test_that("translator_write() serializes tr", {
    temp_file <- withr::local_tempfile(pattern = "_translator_", fileext = ".yml")
    withr::defer(file.remove(translations_paths(tr, dirname(temp_file))))
    translator_write(tr, temp_file, verbose = FALSE)

    expect_snapshot_file(
        temp_file,
        name    = "translator-write-translator.yml",
        compare = testthat::compare_file_text)
})

test_that("translator_write() serializes translations", {
    temp_file <- withr::local_tempfile(pattern = "_translator_", fileext = ".yml")
    withr::defer(file.remove(translations_files))
    translations_files <- translations_paths(tr, dirname(temp_file))
    translator_write(tr, temp_file, verbose = FALSE)

    expect_snapshot_file(translations_files[["el"]], "translator-write-el.txt")
    expect_snapshot_file(translations_files[["es"]], "translator-write-es.txt")
    expect_snapshot_file(translations_files[["fr"]], "translator-write-fr.txt")
})

test_that("translator_write() outputs basic information if verbose is true", {
    # dirname() returns "." when a single filename is
    # passed to it (with no other reference). This is
    # used below to ensure snapshotted outputs remain
    # constant (no matter the parent directory).
    withr::local_dir(tempdir())
    withr::defer(file.remove(temp_file, translations_paths(tr, getwd())))
    temp_file <- "_translator.yml"

    expect_output(translator_write(tr, temp_file))
    expect_snapshot(translator_write(tr, temp_file, TRUE))
})

test_that("translator_write() does not write translations files if translations is false", {
    temp_file <- withr::local_tempfile(pattern = "_translator_", fileext = ".yml")
    translations_files <- translations_paths(tr, dirname(temp_file))

    # There should be no output.
    expect_silent(translator_write(tr, temp_file, TRUE, translations = FALSE))

    # Translations files should not exist.
    expect_true(all(!file.exists(translations_files)))
})


# translator_read() ------------------------------------------------------------


test_that("translator_read() returns an R6 object of class Translator", {
    withr::local_options(
        transltr.default.path = withr::local_tempfile(
            pattern = "_translator_",
            fileext = ".yml"))
    withr::defer(file.remove(translations_paths(tr)))
    translator_write(tr, verbose = FALSE)

    expect_s3_class(translator_read(verbose = FALSE), "Translator")
})

test_that("translator_read() validates verbose", {
    expect_error(translator_read(verbose = 1L))
    expect_snapshot(translator_read(verbose = 1L), error = TRUE)
})

test_that("translator_read() validates translations", {
    expect_error(translator_read(translations = 1L))
    expect_snapshot(translator_read(translations = 1L), error = TRUE)
})

test_that("translator_read() reads all related translations files", {
    withr::local_options(
        transltr.default.path = withr::local_tempfile(
            pattern = "_translator_",
            fileext = ".yml"))
    withr::defer(file.remove(translations_paths(tr)))
    translator_write(tr, verbose = FALSE)
    tr2 <- translator_read(verbose = FALSE)

    expect_identical(tr$native_languages, tr2$native_languages)
    expect_identical(tr$hashes, tr2$hashes)
    expect_identical(
        tr$get_text("256e0d7")$translations,
        tr2$get_text("256e0d7")$translations)
    expect_identical(
        tr$get_text("2ac373a")$translations,
        tr2$get_text("2ac373a")$translations)
})

test_that("translator_read() outputs basic information if verbose is true", {
    # dirname() returns "." when a single filename is
    # passed to it (with no other reference). This is
    # used below to ensure snapshotted outputs remain
    # constant (no matter the parent directory).
    withr::local_dir(tempdir())
    withr::defer(file.remove(temp_file, translations_paths(tr, getwd())))
    temp_file <- "_translator.yml"
    translator_write(tr, temp_file, verbose = FALSE)

    expect_output(translator_read(temp_file))
    expect_snapshot(translator_read(temp_file))
})

test_that("translator_read() reports errors", {
    # This only relates to reading translations files.

    # Step 1: get absolute paths to invalid files before
    # changing the working directory (temporarily).
    invalid_files <- c(
        get_mock_path(file.path("translator-io", "el-invalid"), "txt"),
        get_mock_path(file.path("translator-io", "es-invalid"), "txt"),
        get_mock_path(file.path("translator-io", "fr-invalid"), "txt"))

    # dirname() returns "." when a single filename is
    # passed to it (with no other reference). This is
    # used below to ensure snapshotted outputs remain
    # constant (no matter the parent directory).
    withr::local_dir(tempdir())
    withr::defer(file.remove(temp_file, translations_paths(tr, getwd())))
    temp_file <- "_translator.yml"

    # Step 2: export the Translator object and its translations.
    translator_write(tr, temp_file, verbose = FALSE)

    # Step 3: overwrite translations files with invalid files.
    file.copy(
        from      = invalid_files,
        to        = translations_paths(tr, getwd()),
        overwrite = TRUE)

    # Step 4: check how errors are reported.
    expect_output(translator_read(temp_file))
    expect_snapshot(translator_read(temp_file))

    expect_error(translator_read(temp_file, verbose = FALSE))
    expect_snapshot(translator_read(temp_file, verbose = FALSE), error = TRUE)
})

test_that("translator_read() does not read translations files if translations is false", {
    withr::local_options(
        transltr.default.path = withr::local_tempfile(
            pattern = "_translator_",
            fileext = ".yml"))
    translator_write(tr, verbose = FALSE)

    # There should be no output.
    expect_silent(translator_read(translations = FALSE))

    # There should be no translations (aside from source text).
    expect_identical(translator_read(translations = FALSE)$languages, "en")
})


# translations_write() ---------------------------------------------------------


test_that("translations_write() returns null invisibly", {
    temp_file <- withr::local_tempfile(pattern = "el_", fileext = ".txt")

    expect_null(translations_write(tr, temp_file, "el"))
    expect_invisible(translations_write(tr, temp_file, "el"))
})

test_that("translations_write() includes comments", {
    temp_file <- withr::local_tempfile(pattern = "el_", fileext = ".txt")
    translations_write(tr, temp_file, "el")
    text <- text_read(temp_file)

    expect_identical(
        text[seq_along(comments_translations_file)],
        comments_translations_file)
})

test_that("translations_write() serializes translations", {
    temp_file <- withr::local_tempfile(pattern = "el_", fileext = ".txt")
    translations_write(tr, temp_file, "el")

    expect_snapshot_file(temp_file, "translations-write-el.txt")
})


# translations_read() ----------------------------------------------------------


test_that("translations_read() returns a S3 object of class ExportedTranslations", {
    temp_file <- withr::local_tempfile(pattern = "el_", fileext = ".txt")
    translations_write(tr, temp_file, "el")

    expect_s3_class(translations_read(temp_file), "ExportedTranslations")
})

test_that("translations_read() registers translations in tr", {
    # Step 1: export, and write 'fr' translations of tr.
    temp_file <- withr::local_tempfile(pattern = "fr_", fileext = ".txt")
    translations_write(tr, temp_file, "fr")

    # Step 2: create a new Translator object with no 'fr' translations.
    tr2 <- translator(
        id = "test-translator",
        en = "English",
        text(en = "Hello, world!"),
        text(en = "Farewell, world!"))

    # Step 3: call translations_read() and pass the new Translator to it.
    translations_read(temp_file, tr = tr2)

    # Step 4: check that translations were read AND registered.
    expect_identical(tr2$native_languages[["fr"]], "Français")
    expect_identical(tr2$get_translation("256e0d7", "fr"), "Bonjour, monde!")
    expect_identical(tr2$get_translation("2ac373a", "fr"), "Au revoir, monde!")
})
