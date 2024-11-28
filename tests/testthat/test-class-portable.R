language_source_set("en")
withr::defer(language_source_set(NULL))

# Small helper function to scrub date and time from
# field generated_on of PortableTranslator objects.
scrub_date_time <- function(x = character()) {
    placeholder <- "generated_on: January 01, 1900 @ 00:00:00 (UTC)"
    return(gsub("^generated_on:.*", placeholder, x))
}

loc <- location("test-file", 1L, 2L, 3L, 4L)

txt <- text(
    en = "Hello, world!",
    fr = "Bonjour, monde!",
    location("test-file-1", 1L, 2L, 3L, 4L),
    location("test-file-2", 1L, 2L, 3L, 4L),
    hash_algorithm = "utf8")

trans <- translator(
    id = nil_uuid <- "00000000-0000-0000-0000-000000000000",
    en = "English",
    fr = "Français",
    text(
        location("a", 1L, 2L, 3L, 4L),
        en = "Hello, world!",
        fr = "Bonjour, monde!"),
    text(
        location("b", 1L, 2L, 3L, 4L),
        en = "Farewell, world!",
        fr = "Au revoir, monde!"))

portable_loc   <- portable_location(loc)
portable_txt   <- portable_text(txt)
portable_trans <- portable_translator(trans)

# This random Portable object is used with
# Portable versions of loc, txt, and trans
# (see above) to validate format() and
# print() methods.
portable_person <- portable(
    super = "PortablePerson",
    tag   = "Person",
    x     = list(
        FirstName = "John",
        LastName  = "Doe",
        Address   = list(
            StreetAddress = "123 Main Street",
            City          = "Montreal",
            Province      = "Quebec",
            PostalCode    = "H0H 0H0"),
        Notes = c("Send mail to", "address above.")))


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


# portable_translator() --------------------------------------------------------


test_that("portable_translator() returns an S3 object of class PortableTranslator", {
    out     <- portable_translator(trans)
    n_texts <- sum(vapply_1l(lapply(out, attr, which = "tag"), identical, y = "Text"))

    expect_s3_class(out, "PortableTranslator")
    expect_identical(attr(out, "tag"), "Translator")

    # Check informational fields.
    expect_identical(out$version, 1L)
    expect_identical(out$generated_by, constant("generated-by"))
    expect_type(out$generated_on, "character")  # date/time always changes
    expect_length(out$generated_on, 1L)

    # Check field stemming from the input Translator object.
    expect_identical(out$identifier,      trans$id)
    expect_identical(out$hash_algorithm,  trans$hash_algorithm)
    expect_identical(out$source_language, trans$source_langs)
    expect_identical(out$languages,       as.list(trans$native_languages))

    # Fields specific to PortableTranslator objects.
    expect_identical(out$translations_files, list("fr.txt"), ignore_attr = TRUE)
    expect_named(out$translations_files, "fr")

    # Further PortableText objects.
    expect_s3_class(out$`2ac373a`, "PortableText")
    expect_s3_class(out$`256e0d7`, "PortableText")
    expect_identical(n_texts, 2L)
})

test_that("portable_translator() validates x", {
    expect_error(portable_translator(1L))
    expect_snapshot(portable_translator(1L), error = TRUE)
})

test_that("portable_translator() throws an error if there are multiple source languages", {
    trans <- translator(
        en = "English",
        fr = "Français",
        text(en = "Hello, world!",   source_lang = "en"),
        text(fr = "Bonjour, monde!", source_lang = "fr"))

    expect_error(portable_translator(trans))
    expect_snapshot(portable_translator(trans), error = TRUE)
})

test_that("portable_translator() adds tag TranslationsFiles to translations_files", {
    expect_identical(
        attr(portable_translator(trans)$translations_files, "tag"),
        "TranslationsFiles")
})


# portable_text() --------------------------------------------------------------


test_that("portable_text() returns an S3 object of class PortableText", {
    out <- portable_text(txt)

    expect_s3_class(out, "PortableText")
    expect_identical(attr(out, "tag"), "Text")
    expect_identical(out$hash,            txt$hash)
    expect_identical(out$hash_algorithm,  txt$hash_algorithm)
    expect_identical(out$source_language, txt$source_lang)
    expect_identical(out$source_text,     txt$source_text)

    # $locations should not have names.
    expect_identical(out$locations, list(
        portable_location(location("test-file-1", 1L, 2L, 3L, 4L)),
        portable_location(location("test-file-2", 1L, 2L, 3L, 4L))))

    # $translations is defined but NULL by default.
    expect_in("translations", names(out))
    expect_null(out$translations)
})

test_that("portable_text() validates x", {
    expect_error(portable_text(1L))
    expect_snapshot(portable_text(1L), error = TRUE)
})

test_that("portable_text() validates set_translations", {
    expect_error(portable_text(Text$new(), 1L))
    expect_snapshot(portable_text(Text$new(), 1L), error = TRUE)
})

test_that("portable_text() sets translations if set_translations is true", {
    out <- portable_text(txt, TRUE)
    expect_identical(out$translations, list(fr = "Bonjour, monde!"))
})

test_that("portable_text() wraps source text and translations", {
    # The following strings are longer than 80 characters on purpose.
    txt <- text(
        en = "Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.",
        fr = "Le Lorem Ipsum est le faux texte standard de l'imprimerie depuis les années 1500, quand un imprimeur anonyme assembla ensemble des morceaux de texte pour réaliser un livre spécimen de polices de texte.")
    out <- portable_text(txt, TRUE)

    expect_length(out$source_language, 1L)
    expect_length(out$translations$fr, 1L)
    expect_snapshot({
        out$source_text
        out$translations$fr
    })
})

test_that("portable_text() updates hash, source_language, and source_text only if hash is set", {
    out <- portable_text(Text$new())

    expect_null(out$hash)
    expect_null(out$source_language)
    expect_null(out$source_text)
})


# portable_location() ----------------------------------------------------------


test_that("portable_location() returns an S3 object of class PortableLocation", {
    out <- portable_location(loc)

    expect_s3_class(out, "PortableLocation")
    expect_identical(attr(out, "tag"), "Location")
    expect_identical(out$path, loc$path)
    expect_identical(out$ranges, "line 1, column 2 @ line 3, column 4")
})

test_that("portable_location() validates x", {
    expect_error(portable_location(1L))
    expect_snapshot(portable_location(1L), error = TRUE)
})


# format() ---------------------------------------------------------------------


test_that("format.Portable() returns a character string", {
    fmt <- format(portable_person)

    expect_type(fmt, "character")
    expect_length(fmt, 1L)

    # This checks if fmt is a valid YAML string.
    # It is considered valid if the YAMl parser
    # does not throw errors or warnings. See
    # print() for an additional manual check.
    expect_no_condition(yaml::yaml.load(fmt))
})

test_that("format.PortableTranslator() returns the output of format.Portable()", {
    fmt <- format(portable_trans)
    expect_identical(fmt, format.Portable(portable_trans))
})

test_that("format.PortableTranslator() returns a character vector if set_instructions is true", {
    fmt <- format(portable_trans, TRUE)

    expect_type(fmt, "character")
    expect_length(fmt, 2L)

    # The exact contents of instructions (fmt[[1L]])
    # is checked manually below with print() tests.
    expect_type(fmt[[1L]], "character")
    expect_length(fmt[[1L]], 1L)

    expect_identical(fmt[[2L]], format.Portable(portable_trans))
})

test_that("format.PortableTranslator() validates set_instructions", {
    expect_error(format(portable_trans, 1L))
    expect_snapshot(format(portable_trans, 1L), error = TRUE)
})


# print.Portable() -------------------------------------------------------------


test_that("print() works", {
    expect_output(print(portable_person))
    expect_snapshot({
        "Manually check if output below is a YAML serialization."
        "Output should:"
        "  1. begin with a !<Person> YAML tag,"
        "  2. Use a key: value notation,"
        "  3. Use unquoted strings (if possible), and"
        "  4. Use indentation to indicate structure."
        "If these 4 characteristics are observed, the"
        "output is highly likely to be a YAML string."
        print(portable_person)
    })
})

test_that("print() returns x invisibly", {
    withr::local_output_sink(tempfile())

    expect_invisible(print(portable_person))
    expect_identical(print(portable_person), portable_person)
})

test_that("print() works with PortableTranslator objects", {
    expect_snapshot(
        print(portable_translator(Translator$new(nil_uuid))),
        transform = scrub_date_time)
    expect_snapshot(
        print(portable_trans, TRUE),
        transform = scrub_date_time)
})

test_that("print() works with PortableText objects", {
    expect_snapshot(print(portable_text(Text$new())))
    expect_snapshot(print(portable_txt))
})

test_that("print() works with PortableLocation objects", {
    # There is no standardized way to create "empty"
    # Location objects. Such objects are not tested.
    expect_snapshot(print(portable_loc))
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
