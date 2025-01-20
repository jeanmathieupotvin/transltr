language_source_set("en")
withr::defer(language_source_set(NULL))

loc1 <- location("a", 1L, 2L, 3L, 4L)
loc2 <- location("b", 5L, 6L, 7L, 8L)

txt1 <- text(
    loc1,
    en = "Hello, world!",
    es = "¡Hola Mundo!",
    fr = "Bonjour, monde!")
txt2 <- text(
    loc2,
    en = "Farewell, world!",
    fr = "Au revoir, monde!")

tr <- translator(
    id = "test-translator",
    el = "Ελληνικά",
    en = "English",
    es = "Español",
    fr = "Français",
    txt1,
    txt2)


# export() ---------------------------------------------------------------------


test_that("export() works", {
    expect_s3_class(export(location()),       "ExportedLocation")
    expect_s3_class(export(Text$new()),       "ExportedText")
    expect_s3_class(export(Translator$new()), "ExportedTranslator")
})


# export.Location() ------------------------------------------------------------


test_that("export.Location() returns an object of S3 class ExportedLocation", {
    out <- export(loc1, id = "test-id")

    expect_s3_class(out, "ExportedLocation")
    expect_identical(attr(out, "tag"), "Location")

    expect_type(out, "list")
    expect_length(out, 3L)
    expect_named(out, c("Identifier", "Path", "Ranges"))

    expect_identical(out$Identifier, "test-id")
    expect_identical(out$Path, loc1$path)
    expect_identical(out$Ranges, range_format(loc1))
})

test_that("export.Location() validates id", {
    expect_error(export(loc1, 1L))
    expect_snapshot(export(loc1, 1L), error = TRUE)
})


# export.Text() ----------------------------------------------------------------


test_that("export.Text() returns an object of S3 class ExportedText", {
    out    <- export(txt1, id = "test-id")
    loc_id <- sprintf("test-id:%s", loc1$path)

    expect_s3_class(out, "ExportedText")
    expect_identical(attr(out, "tag"), "Text")

    expect_type(out, "list")
    expect_length(out, 7L)
    expect_named(out, c(
        "Identifier",
        "Algorithm",
        "Hash",
        "Source Language",
        "Source Text",
        "Translations",
        "Locations"))

    expect_identical(out$Identifier, "test-id")
    expect_identical(out$Algorithm, txt1$algorithm)
    expect_identical(out$Hash, txt1$hash)
    expect_identical(out$`Source Language`, txt1$source_lang)
    expect_identical(out$`Source Text`, txt1$source_text)
    expect_null(out$Translations)
    expect_identical(out$Locations, list(export(loc1, id = loc_id)))
})

test_that("export.Text() validates id", {
    expect_error(export(txt1, 1L))
    expect_snapshot(export(txt1, 1L), error = TRUE)
})

test_that("export.Text() validates set_translations", {
    expect_error(export(txt1, set_translations = 1L))
    expect_snapshot(export(txt1, set_translations = 1L), error = TRUE)
})

test_that("export.Text() sets Translations if set_translations is true", {
    out   <- export(txt1, set_translations = TRUE)
    trans <- txt1$translations
    trans <- trans[names(trans) != txt1$source_lang]

    expect_identical(out$Translations, as.list(trans))
})

test_that("export.Text() only sets Hash, Source Language, and Source Text if source_lang is set", {
    out <- export(Text$new())

    # Fields are defined, but bound to NULL.
    expect_type(out, "list")
    expect_length(out, 7L)
    expect_named(out, c(
        "Identifier",
        "Algorithm",
        "Hash",
        "Source Language",
        "Source Text",
        "Translations",
        "Locations"))

    expect_null(out$Hash)
    expect_null(out$`Source Language`)
    expect_null(out$`Source Text`)
})

test_that("export.Text() wraps source text and translations longer than 80 chars", {
    txt <- text(
        en = normalize("
            Lorem Ipsum is simply dummy text of the printing and typesetting
            industry. Lorem Ipsum has been the industry's standard dummy text
            ever since the 1500s, when an unknown printer took a galley of type
            and scrambled it to make a type specimen book. It has survived not
            only five centuries, but also the leap into electronic typesetting,
            remaining essentially unchanged. It was popularised in the 1960s with
            the release of Letraset sheets containing Lorem Ipsum passages, and
            more recently with desktop publishing software like Aldus PageMaker
            including versions of Lorem Ipsum."),
        fr = normalize("
            Le Lorem Ipsum est simplement du faux texte employé dans la
            composition et la mise en page avant impression. Le Lorem Ipsum
            est le faux texte standard de l'imprimerie depuis les années 1500,
            quand un imprimeur anonyme assembla ensemble des morceaux de texte
            pour réaliser un livre spécimen de polices de texte. Il n'a pas
            fait que survivre cinq siècles, mais s'est aussi adapté à la
            bureautique informatique, sans que son contenu n'en soit modifié.
            Il a été popularisé dans les années 1960 grâce à la vente de
            feuilles Letraset contenant des passages du Lorem Ipsum, et, plus
            récemment, par son inclusion dans des applications de mise en page
            de texte, comme Aldus PageMaker."))

    out <- export(txt, set_translations = TRUE)

    expect_type(out$`Source Text`, "character")
    expect_true(length(out$`Source Text`) > 1L)
    expect_type(out$`Translations`$fr, "character")
    expect_true(length(out$`Translations`$fr) > 1L)
})


# export.Translator() ----------------------------------------------------------


test_that("export.Translator() returns an object of S3 class ExportedTranslator", {
    out    <- export(tr)
    hashes <- tr$hashes

    expect_s3_class(out, "ExportedTranslator")
    expect_identical(attr(out, "tag"), "Translator")
    expect_type(out, "list")
    expect_length(out, 4L)
    expect_named(out, c("Identifier", "Algorithm", "Languages", "Texts"))

    expect_identical(out$Identifier, tr$id)
    expect_identical(out$Algorithm, tr$algorithm)
    expect_identical(out$Languages, as.list(tr$native_languages))
    expect_identical(out$Texts, list(
        export(txt1, names(hashes)[hashes == txt1$hash]),
        export(txt2, names(hashes)[hashes == txt2$hash])))
})


# serialize() ------------------------------------------------------------------


test_that("serialize() returns a character string", {
    out <- serialize(loc1)

    expect_type(out, "character")
    expect_length(out, 1L)
})

test_that("serialize() adds yaml tags", {
    out_loc <- serialize(loc1)
    out_txt <- serialize(txt1)
    out_tr  <- serialize(tr)

    expect_match(out_loc, "^!<Location>")
    expect_match(out_txt, "^!<Text>")
    expect_match(out_tr,  "^!<Translator>")
})

test_that("serialize() uses expected yaml formatting options", {
    # line.sep, indent, and indent.mapping.sequence
    # are args of yaml::as.yaml() that matter for
    # formatting purposes.
    out <- serialize(txt1, set_translations = TRUE)

    # Line sep used is a single \n.
    expect_match(out, "\nIdentifier")
    expect_match(out, "\nAlgorithm")
    expect_match(out, "\nHash")
    expect_match(out, "\nSource Language")
    expect_match(out, "\nSource Text")
    expect_match(out, "\nTranslations")
    expect_match(out, "\nLocations")
    expect_no_match(out, "\n{2,}")

    # Mappings and sequences are indented using two spaces.
    expect_match(out, "  es:")
    expect_match(out, "  fr:")
    expect_match(out, "  - !<Location>")
})

test_that("serialize() serializes objects as expected", {
    expect_snapshot(cat(serialize(loc1, id = "test-id")))
    expect_snapshot(cat(serialize(txt1, id = "test-id")))
    expect_snapshot(cat(serialize(tr)))
})


# export_translations() --------------------------------------------------------


test_that("export_translations() returns an object of S3 class ExportedTranslations", {
    lang   <- "fr"
    hashes <- tr$hashes
    out    <- export_translations(tr, lang)

    expect_s3_class(out, "ExportedTranslations")
    expect_identical(attr(out, "tag"), "Translations")

    expect_type(out, "list")
    expect_length(out, 5L)
    expect_named(out, c(
        "Identifier",
        "Language Code",
        "Language",
        "Source Language",
        "Translations"))

    expect_identical(out$Identifier, paste(tr$id, "translations", lang, sep = ":"))
    expect_identical(out$`Language Code`, lang)
    expect_identical(out$Language, tr$native_languages[[lang]])
    expect_identical(out$`Source Language`, tr$native_languages[[tr$source_langs]])
    expect_identical(out$Translations, structure(
        list(
            list(
                `Source Text` = txt1$source_text,
                Translation   = txt1$get_translation(lang)),
            list(
                `Source Text` = txt2$source_text,
                Translation   = txt2$get_translation(lang))),
        names = c(
            names(hashes[hashes == txt1$hash]),
            names(hashes[hashes == txt2$hash]))))
})

test_that("export_translations() validates lang", {
    expect_error(export_translations(tr, 1L))
    expect_error(export_translations(tr, "missing-lang"))
    expect_snapshot(export_translations(tr, 1L), error = TRUE)
    expect_snapshot(export_translations(tr, "missing-lang"), error = TRUE)
})

test_that("export_translations() validates tr", {
    tr <- translator(
        id = "test-translator",
        en = "English",
        fr = "Français",
        text(
            source_lang = "en",
            en = "Hello, world!",
            fr = "Bonjour, monde!"),
        text(
            source_lang = "fr",
            en = "Farewell, world!",
            fr = "Au revoir, monde!"))

    expect_error(export_translations(1L))
    expect_error(export_translations(tr, "fr"))
    expect_snapshot(export_translations(1L, "fr"), error = TRUE)
    expect_snapshot(export_translations(tr, "fr"), error = TRUE)
})

test_that("export_translations() sets each translation equal to constant 'untranslated' if required", {
    out <- export_translations(tr, "el")

    expect_identical(out$Translations$`256e0d7`$Translation, constant("untranslated"))
    expect_identical(out$Translations$`2ac373a`$Translation, constant("untranslated"))
})


# serialize_translations() -----------------------------------------------------


test_that("serialize_translations() returns a character string", {
    out <- serialize_translations(tr, "fr")

    expect_type(out, "character")
    expect_length(out, 1L)
})

test_that("serialize() uses expected flat formatting options", {
    # The only formatting option to
    # check is the label.sep argument.
    out <- serialize_translations(tr, "fr")

    expect_match(out, ":: Translations: 256e0d7: Source Text")
    expect_match(out, ":: Translations: 256e0d7: Translation")
})

test_that("serialize_translations() serializes translations as expected", {
    expect_snapshot(cat(serialize_translations(tr, "el"), "\n"))
    expect_snapshot(cat(serialize_translations(tr, "fr"), "\n"))
})


# format_errors() --------------------------------------------------------------


test_that("format_errors() returns a character if throw_error is false", {
    out <- format_errors("", throw_error = FALSE)

    expect_type(out, "character")
    expect_length(out, 1L)
})

test_that("format_errors() throws an error if throw_error is true", {
    expect_error(format_errors(""))
})

test_that("format_errors() validates errors", {
    expect_error(format_errors(1L))
    expect_snapshot(format_errors(1L), error = TRUE)
})

test_that("format_errors() validates throw_error", {
    expect_error(format_errors("", throw_error = 1L))
    expect_snapshot(format_errors("", throw_error = 1L), error = TRUE)
})

test_that("format_errors() substitutes null value passed to id", {
    expect_identical(format_errors("", NULL, FALSE), "['<unknown>'] ")
})

test_that("format_errors() converts non-character values passed to id", {
    expect_identical(format_errors("", 111L, FALSE), "['111'] ")
})

test_that("format_errors() formats errors as expected", {
    errors <- c(
        "'Hash' must be a null, or a non-empty character string.",
        "'Hash' is defined but not 'Source Text', and/or 'Source Lang'.")
    expected <- c(
        "['test-id'] 'Hash' must be a null, or a non-empty character string.",
        "['test-id'] 'Hash' is defined but not 'Source Text', and/or 'Source Lang'.")

    # throw_error is FALSE.
    expect_identical(format_errors(errors, "test-id", FALSE), expected)

    # throw_error is TRUE.
    expect_error(format_errors(errors, "test-id"))
    expect_snapshot(format_errors(errors, "test-id"), error = TRUE)
})


# assert.ExportedLocation() ----------------------------------------------------


test_that("assert.ExportedLocation() returns a character if x is valid", {
    out <- assert(export(loc1))

    expect_type(out, "character")
    expect_length(out, 0L)
})

test_that("assert.ExportedLocation() returns a character if x is invalid and throw_error is false", {
    # By creating an integer of class ExportedLocation, we
    # simultaneously check that (1) it is replaced by an
    # empty list, and (2) all errors are accumulated and
    # reported.
    invalid <- structure(1L, class = "ExportedLocation")
    out     <- assert(invalid, FALSE)

    expect_identical(out, c(
        "['<unknown>'] 'Path' must be a non-empty character string.",
        "['<unknown>'] 'Ranges' must be a single `Ln <int>, Col <int> @ Ln <int>, Col <int>` character string, or a sequence of such values."))
})

test_that("assert.ExportedLocation() throws an error if x is invalid and throw_error is true", {
    invalid <- structure(1L, class = "ExportedLocation")

    expect_error(assert(invalid))
    expect_snapshot(assert(invalid), error = TRUE)
})

test_that("assert.ExportedLocation() detects invalid Path field", {
    out <- export(loc1, id = "test-id")
    out$Path <- 1L

    expect_error(assert(out))
    expect_snapshot(assert(out), error = TRUE)
})


test_that("assert.ExportedLocation() detects invalid Ranges field", {
    out <- export(loc1, id = "test-id")
    out$Ranges <- c(
        "Ln 1, Col 2 @ Ln 3, Col  4",
        "Ln X, Col 2 @@ Ln 3, Col 4")

    expect_error(assert(out))
    expect_snapshot(assert(out), error = TRUE)
})


# assert.ExportedText() --------------------------------------------------------


test_that("assert.ExportedText() returns a character if x is valid", {
    out <- assert(export(txt1))

    expect_type(out, "character")
    expect_length(out, 0L)
})

test_that("assert.ExportedText() returns a character if x is invalid and throw_error is false", {
    # By creating an integer of class ExportedText, we
    # simultaneously check that (1) it is replaced by
    # an empty list, and (2) all errors are accumulated
    # and reported.
    invalid <- structure(1L, class = "ExportedText")
    out     <- assert(invalid, FALSE)

    expect_identical(out, c(
        "['<unknown>'] 'Algorithm' must be equal to 'sha1', or 'utf8'.",
        "['<unknown>'] 'Hash' must be a null, or a non-empty character string.",
        "['<unknown>'] 'Source Language' must be a null, or a non-empty character string.",
        "['<unknown>'] 'Source Text' must be a null, or a non-empty character string.",
        "['<unknown>'] 'Translations' must be a null, or a mapping of non-empty character strings.",
        "['<unknown>'] 'Locations' must be a sequence of 'Location' objects."))
})

test_that("assert.ExportedText() throws an error if x is invalid and throw_error is true", {
    invalid <- structure(1L, class = "ExportedText")

    expect_error(assert(invalid))
    expect_snapshot(assert(invalid), error = TRUE)
})

test_that("assert.ExportedText() detects invalid Algorithm field", {
    out <- export(txt1, id = "test-id")
    out$Algorithm <- 1L

    expect_error(assert(out))
    expect_snapshot(assert(out), error = TRUE)
})

test_that("assert.ExportedText() detects invalid Hash field", {
    out1 <- export(txt1, id = "test-id")
    out2 <- out1
    out3 <- export(Text$new(), id = "test-id")

    # Hash is missing.
    out1$Hash <- NULL

    # Hash is not a character string.
    out2$Hash <- 1L

    # Hash is defined, but not Source Text, and Source Language.
    out3$Hash <- "256e0d707386d0fcd9abf10ad994000bdaa25812"

    expect_error(assert(out1))
    expect_error(assert(out2))
    expect_error(assert(out3))
    expect_snapshot(assert(out1), error = TRUE)
    expect_snapshot(assert(out3), error = TRUE)
})

test_that("assert.ExportedText() detects invalid Source Language field", {
    out1 <- export(txt1, id = "test-id")
    out2 <- out1

    # Source Language is missing.
    out1$`Source Language` <- NULL

    # Source Language is not a character string.
    out2$`Source Language` <- 1L

    expect_error(assert(out1))
    expect_error(assert(out2))
    expect_snapshot(assert(out1), error = TRUE)
})

test_that("assert.ExportedText() detects invalid Source Text field", {
    out1 <- export(txt1, id = "test-id")
    out2 <- out1
    out3 <- out1
    out4 <- out1

    # Source Text is missing.
    out1$`Source Text` <- NULL

    # Source Text is not a character string.
    out2$`Source Text` <- 1L

    # Source Text is not defined, but Source Language is.
    # The former is removed, then replaced by a NULL.
    out3$`Source Text` <- NULL
    out3 <- structure(
        c(out3, list(`Source Text` = NULL)),
        class = "ExportedText")

    # Source Language is not defined, but Source Text is.
    # The former is removed, then replaced by a NULL.
    out4$`Source Language` <- NULL
    out4 <- structure(
        c(out3, list(`Source Language` = NULL)),
        class = "ExportedText")

    expect_error(assert(out1))
    expect_error(assert(out2))
    expect_error(assert(out3))
    expect_error(assert(out4))
    expect_snapshot(assert(out1), error = TRUE)
    expect_snapshot(assert(out2), error = TRUE)
})

test_that("assert.ExportedText() detects invalid Translations field", {
    out1 <- export(txt1, id = "test-id")
    out2 <- out1
    out3 <- out1
    out4 <- out1

    # Translations is missing.
    out1$Translations <- NULL

    # Translations is not a list.
    out2$Translations <- 1L

    # Translations is a list, but not named.
    out3$Translations <- list("a")

    # Translations is a named list, but
    # does not contain character strings.
    out4$Translations <- list(a = "a", b = 1L)

    expect_error(assert(out1))
    expect_error(assert(out2))
    expect_error(assert(out3))
    expect_error(assert(out4))
    expect_snapshot(assert(out1), error = TRUE)
})

test_that("assert.ExportedText() detects invalid Locations field", {
    out1 <- export(txt1, id = "test-id")
    out2 <- out1
    out3 <- out1

    # Locations is not a list.
    out1$Locations <- 1L

    # Locations is a list, but contains objects
    # that are not of class ExportedLocation.
    out2$Locations <- list("a", loc1, loc2)

    # Translations is a named list of
    # invalid ExportedLocation objects.
    out3$Locations <- list(
        export(loc1, id = "test-id"),
        export(loc2, id = "test-id"))
    out3$Locations[[1L]]$Path   <- 1L
    out3$Locations[[2L]]$Ranges <- 1L

    expect_error(assert(out1))
    expect_error(assert(out2))
    expect_error(assert(out3))
    expect_snapshot(assert(out1), error = TRUE)
    expect_snapshot(assert(out3), error = TRUE)
})


# assert.ExportedTranslator() --------------------------------------------------


test_that("assert.ExportedTranslator() returns a character if x is valid", {
    out <- assert(export(tr))

    expect_type(out, "character")
    expect_length(out, 0L)
})

test_that("assert.ExportedTranslator() returns a character if x is invalid and throw_error is false", {
    # By creating an integer of class ExportedTranslator,
    # we simultaneously check that (1) it is replaced by
    # an empty list, and (2) all errors are accumulated
    # and reported.
    invalid <- structure(1L, class = "ExportedTranslator")
    out     <- assert(invalid, FALSE)

    expect_identical(out, c(
        "['<unknown>'] 'Identifier' must be a non-empty character string.",
        "['<unknown>'] 'Algorithm' must be equal to 'sha1', or 'utf8'.",
        "['<unknown>'] 'Languages' must a mapping of non-empty character strings.",
        "['<unknown>'] 'Texts' must a sequence of 'Text' objects."))
})

test_that("assert.ExportedTranslator() throws an error if x is invalid and throw_error is true", {
    invalid <- structure(1L, class = "ExportedTranslator")

    expect_error(assert(invalid))
    expect_snapshot(assert(invalid), error = TRUE)
})

test_that("assert.ExportedTranslator() detects invalid Identifier field", {
    out <- export(tr)
    out$Identifier <- 1L

    expect_error(assert(out))
    expect_snapshot(assert(out), error = TRUE)
})

test_that("assert.ExportedTranslator() detects invalid Algorithm field", {
    out <- export(tr)
    out$Algorithm <- 1L

    expect_error(assert(out))
    expect_snapshot(assert(out), error = TRUE)
})

test_that("assert.ExportedTranslator() detects invalid Languages field", {
    out1 <- export(tr)
    out2 <- out1
    out3 <- out1

    # Languages is not a list.
    out1$Languages <- 1L

    # Languages is a list, but not named.
    out2$Languages <- list("a")

    # Languages is a named list, but
    # does not contain character strings.
    out3$Languages <- list(a = "a", b = 1L)

    expect_error(assert(out1))
    expect_error(assert(out2))
    expect_error(assert(out3))
    expect_snapshot(assert(out1), error = TRUE)
})

test_that("assert.ExportedTranslator() detects invalid Texts field", {
    out1 <- export(tr)
    out2 <- out1
    out3 <- out1

    # Texts is not a list.
    out1$Texts <- 1L

    # Texts is a list, but contains objects
    # that are not of class ExportedText.
    out2$Texts <- list("a", txt1, txt2)

    # Texts is a named list of
    # invalid ExportedTexts objects.
    out3$Texts <- list(
        export(txt1, id = "test-id"),
        export(txt2, id = "test-id"))
    out3$Texts[[1L]]$Hash      <- 1L
    out3$Texts[[2L]]$Algorithm <- 1L

    expect_error(assert(out1))
    expect_error(assert(out2))
    expect_error(assert(out3))
    expect_snapshot(assert(out1), error = TRUE)
    expect_snapshot(assert(out3), error = TRUE)
})


# assert.ExportedTranslations() ------------------------------------------------


test_that("assert.ExportedTranslations() returns a character if x is valid", {
    out <- assert(export_translations(tr, "fr"))

    expect_type(out, "character")
    expect_length(out, 0L)
})

test_that("assert.ExportedTranslations() returns a character if x is invalid and throw_error is false", {
    # By creating an integer of class ExportedTranslations,
    # we simultaneously check that (1) it is replaced by an
    # empty list, and (2) all errors are accumulated and
    # reported.
    invalid <- structure(1L, class = "ExportedTranslations")
    out     <- assert(invalid, FALSE)

    expect_identical(out, c(
        "['<unknown>'] 'Identifier' must be a non-empty character string.",
        "['<unknown>'] 'Language Code' must be a non-empty character string.",
        "['<unknown>'] 'Language' must be a non-empty character string.",
        "['<unknown>'] 'Source Language' must be a non-empty character string.",
        "['<unknown>'] 'Translations' must be a sequence of 'Source Text', and 'Translation' sections."))
})

test_that("assert.ExportedTranslations() throws an error if x is invalid and throw_error is true", {
    invalid <- structure(1L, class = "ExportedTranslations")

    expect_error(assert(invalid))
    expect_snapshot(assert(invalid), error = TRUE)
})

test_that("assert.ExportedTranslations() detects invalid Identifier field", {
    out <- export_translations(tr, "fr")
    out$Identifier <- 1L

    expect_error(assert(out))
    expect_snapshot(assert(out), error = TRUE)
})

test_that("assert.ExportedTranslations() detects invalid Language Code field", {
    out <- export_translations(tr, "fr")
    out$`Language Code` <- 1L

    expect_error(assert(out))
    expect_snapshot(assert(out), error = TRUE)
})

test_that("assert.ExportedTranslations() detects invalid Language field", {
    out <- export_translations(tr, "fr")
    out$Language <- 1L

    expect_error(assert(out))
    expect_snapshot(assert(out), error = TRUE)
})

test_that("assert.ExportedTranslations() detects invalid Source Language field", {
    out <- export_translations(tr, "fr")
    out$`Source Language` <- 1L

    expect_error(assert(out))
    expect_snapshot(assert(out), error = TRUE)
})

test_that("assert.ExportedTranslations() detects invalid Translations field", {
    out1 <- export_translations(tr, "fr")
    out2 <- out1
    out3 <- out1

    # Translations is not a list.
    out1$Translations <- 1L

    # Translations is a list, but not named.
    out2$Translations <- list(list(`Source Text` = "a", Translation = "a"))

    # Translations is a named list, but contains
    # Source Text (sub-child) elements that are
    # not character strings.
    out3$Translations <- list(a = list(`Source Text` = 1L, Translation = "a"))

    expect_error(assert(out1))
    expect_error(assert(out2))
    expect_error(assert(out3))
    expect_snapshot(assert(out1), error = TRUE)
    expect_snapshot(assert(out2), error = TRUE)
    expect_snapshot(assert(out3), error = TRUE)
})


# import() ---------------------------------------------------------------------


test_that("import() works", {
    expect_s3_class(import(export(location())),       "Location")
    expect_s3_class(import(export(Text$new())),       "Text")
    expect_s3_class(import(export(Translator$new())), "Translator")
})

test_that("import() calls assert() before dispatching", {
    loc <- location("a", 1L, 2L, 3L, 4L)
    invalid      <- export(loc)
    invalid$Path <- 1L

    expect_error(import(invalid))
})


# import.default() -------------------------------------------------------------


test_that("import.default() works", {
    expect_error(import(1L))
    expect_snapshot(import(1L), error = TRUE)
})


# import.ExportedLocation() ----------------------------------------------------


test_that("import.ExportedLocation() returns an object of R6 class Location", {
    out <- import(export(loc1))

    expect_s3_class(out, "Location")
    expect_identical(out, loc1)
})

test_that("import.ExportedLocation() handles multiple ranges", {
    loc <- location("a", c(1L, 5L), c(2L, 6L), c(3L, 7L), c(4L, 8L))

    expect_identical(import(export(loc)), loc)
})


# import.ExportedText() --------------------------------------------------------


test_that("import.ExportedText() returns an object of R6 class Text", {
    out <- import(export(txt1, set_translations = TRUE))

    expect_s3_class(out, "Text")
    expect_identical(out, txt1)
})

test_that("import.ExportedText() only set source_lang and source_text if they are not null", {
    txt <- Text$new()
    import(export(txt, set_translations = TRUE))

    expect_identical(import(export(txt, set_translations = TRUE)), txt)
})

test_that("import.ExportedText() throws a warning if hash is invalid", {
    etxt1 <- export(txt1, "test-id", set_translations = TRUE)
    etxt1$Hash <- "invalidhash"

    expect_warning(import(etxt1))
    expect_snapshot(import(etxt1))
})


# import.ExportedTranslator() --------------------------------------------------


test_that("import.ExportedTranslator() returns an object of R6 class Translator", {
    out <- import(export(tr, set_translations = TRUE))

    expect_s3_class(out, "Translator")
    expect_identical(out, tr)
})


# deserialize() ----------------------------------------------------------------


test_that("deserialize() returns an object of R6 class Location, Text, or Translator", {
    # deserialize() returns the output of import().
    expect_s3_class(deserialize(serialize(location())),       "Location")
    expect_s3_class(deserialize(serialize(Text$new())),       "Text")
    expect_s3_class(deserialize(serialize(Translator$new())), "Translator")
})

test_that("deserialize() validates string", {
    expect_error(deserialize(1L))
    expect_snapshot(deserialize(1L), error = TRUE)
})

test_that("deserialize() throws an error when string is an invalid yaml object", {
    expect_error(deserialize("a: 1\nb 2\n"))
    expect_snapshot(deserialize("a: 1\nb 2\n"), error = TRUE)
})


# import.ExportedTranslations() ------------------------------------------------


test_that("import.ExportedTranslations() returns an object of S3 class ExportedTranslations", {
    trans <- export_translations(tr, "fr")
    out   <- import(trans)

    expect_s3_class(out, "ExportedTranslations")
    expect_identical(out, trans)
})

test_that("import.ExportedTranslations() replaces empty translations with a constant", {
    # Language el has no available translation.
    trans1 <- export_translations(tr, "el")
    trans2 <- trans1
    trans2$Translations$`256e0d7`$Translation <- ""
    trans2$Translations$`2ac373a`$Translation <- ""
    out1 <- import(trans1)
    out2 <- import(trans2)

    expect_identical(out1$Translations$`256e0d7`$Translation, constant("empty"))
    expect_identical(out1$Translations$`2ac373a`$Translation, constant("empty"))
    expect_identical(out2$Translations$`256e0d7`$Translation, constant("empty"))
    expect_identical(out2$Translations$`2ac373a`$Translation, constant("empty"))
})

test_that("import.ExportedTranslations() normalizes translations", {
    trans <- export_translations(tr, "el")
    trans$Translations$`256e0d7`$Translation <- "
        Hello,
        world!"
    trans$Translations$`2ac373a`$Translation <- "
        Farewell,
        world!"
    out <- import(trans)

    expect_identical(out$Translations$`256e0d7`$Translation, "Hello, world!")
    expect_identical(out$Translations$`2ac373a`$Translation, "Farewell, world!")
})

test_that("import.ExportedTranslations() validates tr if it is not null", {
    import(export_translations(tr, "fr"), tr)

    expect_error(import(export_translations(tr, "fr"), 1L))
    expect_snapshot(import(export_translations(tr, "fr"), 1L), error = TRUE)
})

test_that("import.ExportedTranslations() registers language if tr is not null and it is not registered", {
    trans <- export_translations(tr, "fr")
    tr2   <- Translator$new()
    tr3   <- Translator$new()
    tr3$set_native_languages(fr = "French")
    import(trans, tr2)
    import(trans, tr3)

    expect_identical(tr2$native_languages, c(fr = tr2$native_languages[["fr"]]))
    expect_identical(tr3$native_languages, c(fr = tr3$native_languages[["fr"]]))
})

test_that("import.ExportedTranslations() registers translations", {
    # Translations are only registered if they
    # correspond to an existing source text.
    lang  <- "fr"
    trans <- export_translations(tr, lang)
    tr2   <- Translator$new()
    tr2$set_text(en = "Hello, world!")
    tr2$set_text(en = "Farewell, world!")
    import(trans, tr2)

    expect_identical(tr2$get_translation("256e0d7", lang), "Bonjour, monde!")
    expect_identical(tr2$get_translation("2ac373a", lang), "Au revoir, monde!")
})


# deserialize_translations() ---------------------------------------------------


test_that("deserialize_translations() returns an object of S3 class ExportedTranslations", {
    # deserialize_translations() returns the output
    # of import() (import.ExportedTranslations()).
    out <- serialize_translations(tr, "fr")

    expect_s3_class(deserialize_translations(out), "ExportedTranslations")
})
