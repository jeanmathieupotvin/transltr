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


test_that("export.Location() returns a S3 object of class ExportedLocation", {
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


test_that("export.Text() returns a S3 object of class ExportedText", {
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


test_that("export.Translator() returns a S3 object of class ExportedTranslator", {
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


test_that("export_translations() returns a S3 object of class ExportedTranslations", {
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

    expect_identical(out$Identifier, tr$id)
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

    expect_error(export_translations(tr, "fr"))
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

