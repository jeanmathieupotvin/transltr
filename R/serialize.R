#' Serialize Objects
#'
#' @description
#' Convert [`Translator`][Translator] objects, [`Text`][Text] objects, and
#' [`Location`][Location] objects to a [YAML][yaml::as.yaml()] object, or
#' vice-versa.
#'
#' Convert translations contained by a [`Translator`][Translator] object to
#' a custom textual representation (a [FLAT][flat_serialize()] object), or
#' vive-versa.
#'
#' @details
#' The information contained within a [`Translator`][Translator] object is
#' split by default. Unless `set_translations` is `TRUE`, translations are
#' serialized independently from other fields. This is useful when creating
#' Translator files and translations files.
#'
#' While [serialize()] and [serialize_translations()] are distinct, they share
#' a common *design* and perform the same *thing*, at least conceptually. The
#' same is true for [deserialize()] and [deserialize_translations()]. These 4
#' functions are those that should be used in almost all circumstances.
#'
#' ## Serialization
#'
#' The data serialization process performed by [serialize()] and
#' [serialize_translations()] is internally broken down into 2 steps: objects
#' are first *exported* before being *serialized*.
#'
#' [export()] and [export_translations()] are *preserializing mechanisms* that
#' convert objects into *transient* objects that ease the conversion process.
#' They are never returned to the user: [serialize()], and
#' [serialize_translations()] immediately transform them into character strings.
#'
#' [serialize()] returns a [YAML object][yaml::as.yaml()].
#'
#' [serialize_translations()] returns a [FLAT object][flat_serialize()].
#'
#' ## Deserialization
#'
#' The data deserialization process performed by [deserialize()] and
#' [deserialize_translations()] is internally broken down into 3 steps: objects
#' are first *deserialized*, then *validated* and finally, *imported*.
#'
#' [deserialize()] and [deserialize_translations()] are
#' *raw deserializer mechanisms*: `string` is converted into an \R named list
#' that is **presumed** to be an *exported* object. [deserialize()] relies on
#' [YAML tags](https://yaml.org/spec/1.1/#id858600) to infer the class of each
#' object.
#'
#' The contents of the *transient* objects is thoroughly checked with an
#' [assert()] method (based on the underlying presumed class). Valid objects
#' are *imported* back into an appropriate \R object with [import()].
#'
#' Custom fields and comments added by users to serialized objects are ignored.
#'
#' ## Formatting errors
#'
#' [assert()] methods accumulate error messages before returning, or throwing
#' them. [format_errors()] is a helper function that eases this process. It
#' exists to avoid repeting code in each method. There is no reason to call
#' it outside of [assert()] methods.
#'
#' @param x Any \R object.
#'
#' @param tr A [`Translator`][Translator] object.
#'
#'   This argument is `NULL` by default for [deserialize_translations()] and
#'   [import.ExportedTranslations()]. If a [`Translator`][Translator] object
#'   is passed to these functions, they will import translations and further
#'   register them (as long as they correspond to an existing source text).
#'
#' @param string A non-empty and non-[NA][base::NA] character string. Contents
#'   to deserialize.
#'
#' @param id A non-empty and non-[NA][base::NA] character string. A unique
#'   identifier for the underlying object. It is used for validation purposes.
#'
#' @param set_translations A non-[NA][base::NA] logical value. Should
#'   translations be included in the resulting [`ExportedText`][export()]
#'   object? If `FALSE`, field `Translations` is set equal to `NULL`.
#'
#' @param ... Further arguments passed to, or from other methods.
#'
#' @param errors A non-empty character vector of non-[NA][base::NA] values.
#'   Error message(s) describing why object(s) are invalid.
#'
#' @template param-lang
#'
#' @template param-throw-error
#'
#' @returns
#' See other sections for further information.
#'
#' [serialize()], and [serialize_translations()] return a character string.
#'
#' [export()] returns a named list of S3 class
#'
#'   * [`ExportedTranslator`][export()] if `x` is a [`Translator`][Translator]
#'     object,
#'   * [`ExportedText`][export()] if `x` is a [`Text`][Text] object, or
#'   * [`ExportedLocation`][export()] if `x` is a [`Location`][Location] object.
#'
#' [export_translations()] returns an [`ExportedTranslations`][export()] object.
#'
#' [deserialize()] and [import()] return
#'
#'   * a [`Translator`][Translator] object if `x` is a valid
#'     [`ExportedTranslator`][export()] object,
#'   * a [`Text`][Text] object if `x` is a valid [`ExportedText`][export()]
#'     object, or
#'   * a [`Location`][Location] object if `x` a valid
#'     [`ExportedLocation`][export()] object.
#'
#' [deserialize_translations()] and [import.ExportedTranslations()] return an
#' [`ExportedTranslations`][export()] object. They further register imported
#' translations if a [`Translator`][Translator] object is passed to `tr`.
#'
#'   * Translations must correspond to an existing source text (a registered
#'     [`Text`][Text] object). Otherwise, they are skipped.
#'   * The value passed to `tr` is updated **by reference** and is not returned.
#'
#' [import.default()] is used for its side-effect of throwing an error for
#' unsupported objects.
#'
#' [assert.ExportedTranslator()],
#' [assert.ExportedText()],
#' [assert.ExportedLocation()], and
#' [assert.ExportedTranslations()] return a character vector, possibly empty.
#' If `throw_error` is `TRUE`, an error is thrown if an object is invalid.
#'
#' [format_errors()] returns a character vector, and outputs its contents as
#' an error if `throw_error` is `TRUE`.
#'
#' @section Exported Objects:
#' An exported object is a named list of S3 class
#' [`ExportedTranslator`][export()],
#' [`ExportedText`][export()],
#' [`ExportedLocation`][export()], or
#' [`ExportedTranslations`][export()] and
#' always having a `tag` attribute whose value is equal to the super-class of
#' `x`.
#'
#' There are four main differences between an object and its *exported*
#' counterpart.
#'
#'   1. Field names are slightly more verbose.
#'   2. Source text is treated independently from translations.
#'   3. Unset fields are set equal to `NULL` (a `~` in YAML).
#'   4. Each object has an `Identifier` used to locate errors.
#'
#' The correspondance between objects is self-explanatory.
#'
#'   * See class [`Translator`][Translator] for more information on class
#'     [`ExportedTranslator`][export()].
#'   * See class [`Text`][Text] for more information on class
#'     [`ExportedText`][export()].
#'   * See class [`Location`][Location] for more information on class
#'     [`ExportedLocation`][export()].
#'
#' You may also explore provided examples below.
#'
#' ## The `ExportedTranslations` Class
#'
#' [`ExportedTranslations`][export()] objects are created from a
#' [`Translator`][Translator] object with [export_translations()]. Their purpose
#' is to restructure translations by language. They are different from other
#' exported objects because there is no corresponding `Translations` class.
#'
#' An [`ExportedTranslations`][export()] object is a named list of S3 class
#' [`ExportedTranslations`][export()] containing the following elements.
#'
#' \describe{
#'   \item{`Identifier`}{The unique identifier of argument `tr`. See
#'     [`Translator$id`][Translator] for more information.}
#'   \item{`Language Code`}{The value of argument `lang`.}
#'   \item{`Language`}{The translation's language. See
#'     [`Translator$native_languages`][Translator] for more information.}
#'   \item{`Source Language`}{The source text's language. See
#'     [`Translator$source_langs`][Translator] for more information.}
#'   \item{`Translations`}{A named list containing further named lists. Each
#'     sublist contains two values:
#'     \describe{
#'       \item{`Source Text`}{A non-empty and non-[NA][base::NA] character
#'         string.}
#'       \item{`Translation`}{A non-empty and non-[NA][base::NA] character
#'         string.}
#'     }
#'     See [`Text$translations`][Text] for more information.}
#' }
#'
#' Unavailable translations are automatically replaced by a placeholder that
#' depends on the *context*:
#'
#'   * [`constant("untranslated")`][constant()] for [export()], and
#'   * [`constant("empty")`][constant()] for [import()].
#'
#' @note
#' Dividing the serialization and deserialization processes into multiple steps
#' helps keeping the underlying functions short, and easier to test.
#'
#' @seealso
#' [Official YAML 1.1 specification](https://yaml.org/spec/1.1/),
#' [yaml::as.yaml()],
#' [yaml::yaml.load()],
#' [flat_serialize()],
#' [flat_deserialize()],
#' [translator_read()],
#' [translator_write()],
#' [translations_read()],
#' [translations_write()]
#'
#' @aliases
#' ExportedTranslator
#' ExportedText
#' ExportedLocation
#' ExportedTranslations
#'
#' @rdname serialize
#' @keywords internal
serialize <- function(x, ...) {
    return(
        yaml::as.yaml(
            export(x, ...),
            line.sep = "\n",
            indent.mapping.sequence = TRUE))
}

#' @rdname serialize
#' @keywords internal
serialize_translations <- function(tr = translator(), lang = "") {
    return(flat_serialize(export_translations(tr, lang)))
}

#' @rdname serialize
#' @keywords internal
deserialize <- function(string = "") {
    assert_chr1(string)

    # Callback function for tryCatch() below.
    # It handles errors, and warnings.
    throw_condition <- \(cond) {
        stopf(
            "while unserializing object: %s",
            # Below we attempt to formulate a coherent
            # sentence from error messages returned by
            # the YAML parser. This is not perfect, but
            # better than nothing.
            gsub("[ \n\t.]*$", ".", tolower(cond$message)))
    }

    obj <- tryCatch(condition = throw_condition, {
        yaml::yaml.load(string,
            # eval.expr is disallowed for security.
            eval.expr      = FALSE,
            as.named.list  = TRUE,
            merge.warning  = TRUE,
            # Classes are inferred from existing tags.
            handlers = list(
                Translator = \(x) structure(x, class = "ExportedTranslator"),
                Text       = \(x) structure(x, class = "ExportedText"),
                Location   = \(x) structure(x, class = "ExportedLocation")))
    })

    return(import(obj))
}

#' @rdname serialize
#' @keywords internal
deserialize_translations <- function(string = "", tr = NULL) {
    obj <- structure(flat_deserialize(string), class = "ExportedTranslations")
    return(import(obj, tr = tr))
}

#' @rdname serialize
#' @keywords internal
export_translations <- function(tr = translator(), lang = "") {
    assert_chr1(lang)

    if (!is_translator(tr)) {
        stops("'tr' must be a 'Translator' object.")
    }
    if (length(source_lang <- tr$source_langs) > 1L) {
        stops("all 'Text' objects of 'tr' must have the same 'source_lang'.")
    }
    if (!is_match(lang, names(native_languages <- tr$native_languages))) {
        stops("'lang' must have a corresponding native language registered in 'tr'.")
    }

    untranslated <- constant("untranslated")
    translations <- lapply(lapply(tr$hashes, tr$get_text), \(txt) {
        return(
            list(
                `Source Text` = txt$source_text,
                Translation   = txt$get_translation(lang) %??% untranslated))
    })

    out <- list(
        Identifier        = sprintf("%s:translations:%s", tr$id, lang),
        `Language Code`   = lang,
        Language          = native_languages[[lang]],
        `Source Language` = native_languages[[source_lang]],
        Translations      = translations)

    return(structure(out, class = "ExportedTranslations", tag = "Translations"))
}

#' @rdname serialize
#' @keywords internal
export <- function(x, ...) {
    UseMethod("export")
}

#' @rdname serialize
#' @keywords internal
#' @export
export.Translator <- function(x, ...) {
    out <- list(
        Identifier = x$id,
        Algorithm  = x$algorithm,
        Languages  = as.list(x$native_languages),
        Texts      = map(
            export,
            x    = lapply(x$hashes, x$get_text),
            id   = names(x$hashes),
            more = list(...)))

    return(structure(out, class = "ExportedTranslator", tag = "Translator"))
}

#' @rdname serialize
#' @keywords internal
#' @export
export.Text <- function(x, id = uuid(), set_translations = FALSE, ...) {
    assert_chr1(id)
    assert_lgl1(set_translations)

    source_is_set <- x$source_lang != constant("unset")
    translations  <- if (set_translations) {
        langs <- names(x$translations)
        trans <- x$translations[langs[-match(x$source_lang, langs, 0L)]]
        lapply(trans, strwrap, width = 80L)
    }

    out <- list(
        Identifier        = id,
        Algorithm         = x$algorithm,
        Hash              = if (source_is_set) x$hash,
        `Source Language` = if (source_is_set) x$source_lang,
        `Source Text`     = if (source_is_set) strwrap(x$source_text, 80L),
        Translations      = translations,
        Locations         = map(
            export,
            x    = x$locations,
            id   = sprintf("%s:%s", id, names(x$locations)),
            more = list(...)))

    return(structure(out, class = "ExportedText", tag = "Text"))
}

#' @rdname serialize
#' @keywords internal
#' @export
export.Location <- function(x, id = uuid(), ...) {
    assert_chr1(id)

    out <- list(
        Identifier = id,
        Path       = x$path,
        Ranges     = range_format(x))

    return(structure(out, class = "ExportedLocation", tag = "Location"))
}

#' @rdname serialize
#' @export
assert.ExportedTranslator <- function(x, throw_error = TRUE, ...) {
    # This prevents any out-of-bound
    # errors that may stem from `[[`.
    if (!is.list(x)) {
        x <- list()
    }

    id    <- x[["Identifier"]]
    algo  <- x[["Algorithm"]]
    langs <- x[["Languages"]]
    texts <- x[["Texts"]]

    # Accumulate error messages.
    errors <- c(
        # Validate Identifier.
        if (!is_chr1(id)) {
            "'Identifier' must be a non-empty character string."
        },
        # Validate Algorithm.
        if (!is_match(algo, constant("algorithms"))) {
            sprintf(
                "'Algorithm' must be equal to %s.",
                str_to(constant("algorithms"), TRUE))
        },
        # Validate Languages.
        if (!is_list(langs, TRUE) ||
            !is_named(langs) ||
            !all(vapply_1l(langs, is_chr1))) {
            "'Languages' must a mapping of non-empty character strings."
        },
        # Validate Texts.
        if (!is_list(texts, TRUE) ||
            !all(vapply_1l(texts, inherits, what = "ExportedText"))) {
            "'Texts' must a sequence of 'Text' objects."
        },
        # Validate contents of each Text object.
        unlist(lapply(texts, assert, throw_error = FALSE, ...))
    )

    if (length(errors)) {
        return(format_errors(errors, id, throw_error))
    }

    return(character())
}

#' @rdname serialize
#' @export
assert.ExportedText <- function(x, throw_error = TRUE, ...) {
    # This prevents any out-of-bound
    # errors that may stem from `[[`.
    if (!is.list(x)) {
        x <- list()
    }

    xnames <- names(x)
    algo   <- x[["Algorithm"]]
    hash   <- x[["Hash"]]
    lang   <- x[["Source Language"]]
    text   <- x[["Source Text"]]
    trans  <- x[["Translations"]]
    locs   <- x[["Locations"]]

    # Accumulate error messages.
    errors <- c(
        # Validate Algorithm.
        if (!is_match(algo, constant("algorithms"))) {
            sprintf(
                "'Algorithm' must be equal to %s.",
                str_to(constant("algorithms"), TRUE))
        },
        # Validate Hash.
        # Hash can be NULL and this is
        # different from a missing field.
        if (!match("Hash", xnames, 0L) || !is.null(hash) && !is_chr1(hash)) {
            "'Hash' must be a null, or a non-empty character string."
        },
        if (!is.null(hash) && (is.null(text) || is.null(lang))) {
            "'Hash' is defined but not 'Source Text', and/or 'Source Lang'."
        },
        # Validate Source Language.
        # Source Language can be NULL and
        # this is different from a missing field.
        if (!match("Source Language", xnames, 0L) ||
            !is.null(lang) && !is_chr1(lang)) {
            "'Source Language' must be a null, or a non-empty character string."
        },
        # Validate Source Text.
        # Source Text can be NULL and this
        # is different from a missing field.
        if (!match("Source Text", xnames, 0L) ||
            !is.null(text) && !is_chr1(text)) {
            "'Source Text' must be a null, or a non-empty character string."
        },
        if (!is.null(lang) && is.null(text) ||
            !is.null(text) && is.null(lang)) {
            "'Source Language' is defined but not 'Source Text', or vice-versa."
        },
        # Validate Translations.
        if (!match("Translations", xnames, 0L) ||
            !is.null(trans) && (
                !is_list(trans, TRUE) ||
                !is_named(trans) ||
                !all(vapply_1l(trans, is_chr1)))) {
            "'Translations' must be a null, or a mapping of non-empty character strings."
        },
        # Validate Locations.
        if (!is_list(locs, TRUE) ||
            !all(vapply_1l(locs, inherits, what = "ExportedLocation"))) {
            "'Locations' must be a sequence of 'Location' objects."
        },
        # Validate contents of each Location object.
        unlist(lapply(locs, assert, throw_error = FALSE, ...))
    )

    if (length(errors)) {
        return(format_errors(errors, x[["Identifier"]], throw_error))
    }

    return(character())
}

#' @rdname serialize
#' @export
assert.ExportedLocation <- function(x, throw_error = TRUE, ...) {
    # This prevents any out-of-bound
    # errors that may stem from `[[`.
    if (!is.list(x)) {
        x <- list()
    }

    path   <- x[["Path"]]
    ranges <- x[["Ranges"]]

    # Accumulate error messages.
    errors <- c(
        # Validate Path.
        if (!is_chr1(path)) {
            "'Path' must be a non-empty character string."
        },
        # Validate Ranges.
        if (!is_chr(ranges) || !all(range_is_parseable(ranges))) {
            sprintf(
                "'Ranges' must be a single %s character string, or a sequence of such values.",
                constant("range-format"))
        }
    )

    if (length(errors)) {
        return(format_errors(errors, x[["Identifier"]], throw_error))
    }

    return(character())
}

#' @rdname serialize
#' @keywords internal
#' @export
assert.ExportedTranslations <- function(x, throw_error = TRUE, ...) {
    # This prevents any out-of-bound
    # errors that may stem from `[[`.
    if (!is.list(x)) {
        x <- list()
    }

    id          <- x[["Identifier"]]
    lang        <- x[["Language Code"]]
    native_lang <- x[["Language"]]
    source_lang <- x[["Source Language"]]
    trans       <- x[["Translations"]]

    # Accumulate error messages.
    errors <- c(
        # Validate Identifier.
        if (!is_chr1(id)) {
            "'Identifier' must be a non-empty character string."
        },
        # Validate Language Code.
        if (!is_chr1(lang)) {
            "'Language Code' must be a non-empty character string."
        },
        # Validate Language.
        if (!is_chr1(native_lang)) {
            "'Language' must be a non-empty character string."
        },
        # Validate Source Language.
        if (!is_chr1(source_lang)) {
            "'Source Language' must be a non-empty character string."
        },
        # Validate Translations.
        if (!is_list(trans) || !is_named(trans)) {
            "'Translations' must be a sequence of 'Source Text', and 'Translation' sections."
        },
        if (!all(vapply_1l(trans, \(x) !match("Source Text", names(x), 0L) || is_chr1(x[["Source Text"]])))) {
            "'Source Text' sections must be character strings. They can also be empty, or missing."
        },
        if (!all(vapply_1l(trans, \(x) match("Translation", names(x), 0L) && is_chr1(x[["Translation"]], TRUE)))) {
            "'Translation' sections must be character strings. They can also be empty, but not missing."
        }
    )

    if (length(errors)) {
        return(format_errors(errors, id, throw_error))
    }

    return(character())
}

#' @rdname serialize
#' @keywords internal
import <- function(x, ...) {
    assert(x, ...)
    UseMethod("import")
}

#' @rdname serialize
#' @keywords internal
#' @export
import.ExportedTranslator <- function(x, ...) {
    tr <- Translator$new(x[["Identifier"]], x[["Algorithm"]])

    do.call(tr$set_native_languages, x[["Languages"]])
    do.call(tr$set_texts, lapply(x[["Texts"]], import, ...))

    return(tr)
}

#' @rdname serialize
#' @keywords internal
#' @export
import.ExportedText  <- function(x, ...) {
    txt <- Text$new(x[["Algorithm"]])

    source_lang <- x[["Source Language"]]
    source_text <- x[["Source Text"]]

    do.call(txt$set_locations, lapply(x[["Locations"]], import, ...))
    do.call(txt$set_translations, x[["Translations"]] %??% list())

    if (!is.null(source_lang) && !is.null(source_text)) {
        txt$set_translation(source_lang, normalize(source_text))
        txt$source_lang <- source_lang
    }

    # NULL hashes are replaced by the 'unset'
    # constant (default value of Text$hash)
    # to ensure proper comparisons. %??% has
    # precedence over !=.
    if (x[["Hash"]] %??% constant("unset") != txt$hash) {
        warn_msg <- sprintf(
            "['%s'] 'Hash' is not equal to computed hash ('%s'). The latter will be used.",
            x[["Identifier"]],
            txt$hash)

        warning(warn_msg, call. = FALSE)
    }

    return(txt)
}

#' @rdname serialize
#' @keywords internal
#' @export
import.ExportedLocation <- function(x, ...) {
    ints <- range_parse(x[["Ranges"]])

    return(
        location(
            x[["Path"]],
            vapply_1i(ints, `[[`, 1L),
            vapply_1i(ints, `[[`, 2L),
            vapply_1i(ints, `[[`, 3L),
            vapply_1i(ints, `[[`, 4L)))
}

#' @rdname serialize
#' @keywords internal
#' @export
import.ExportedTranslations <- function(x, tr = NULL, ...) {
    empty        <- constant("empty")
    untranslated <- constant("untranslated")

    # Set untranslated source texts
    # (empty Translation sections)
    # equal to constant empty.
    trans <- rapply(x[["Translations"]], how = "replace", f = \(txt) {
        if (!nzchar(txt) || txt == untranslated) {
            return(empty)
        }

        return(normalize(txt))
    })

    # Avoid custom fields (possibly added by users) to
    # be included in the ExportedTranslations object.
    out <- structure(
        list(
            Identifier        = x[["Identifier"]],
            `Language Code`   = x[["Language Code"]],
            Language          = x[["Language"]],
            `Source Language` = x[["Source Language"]],
            Translations      = trans),
        class = "ExportedTranslations",
        tag = "Translations")

    if (!is.null(tr)) {
        if (!is_translator(tr)) {
            stops("'tr' must be a 'Translator' object.")
        }

        lang <- x[["Language Code"]]

        if (!match(lang, names(tr$native_languages), 0L)) {
            do.call(
                tr$set_native_languages,
                structure(x["Language"], names = lang))
        }

        # Empty translations are skipped silently.
        # vapply() is used to keep names (reduced
        # hashes) required below.
        texts <- vapply(trans, `[[`, NA_character_, i = "Translation")
        texts <- texts[texts != empty]

        map(hash = names(texts), text = texts, fun = \(hash, text) {
            if (!is.null(txt <- tr$get_text(hash))) {
                txt$set_translation(lang, text)
            }
        })
    }

    return(out)
}

#' @rdname serialize
#' @keywords internal
#' @export
import.default <- function(x, ...) {
    stops(
        "deserialized object is not supported by transltr.",
        " It is likely missing a '!<type>' tag, or has an invalid one.")
}

#' @rdname serialize
#' @keywords internal
format_errors <- function(
    errors      = character(),
    id          = uuid(),
    throw_error = TRUE)
{
    assert_chr(errors)
    assert_lgl1(throw_error)

    if (throw_error) {
        if (length(errors) == 1L) {
            stops(errors)
        }

        # This puts each elements of errors on its own line.
        # The format is as follow.
        # Error:
        #  - Error 1.
        #  - Error 2.
        #  - ...
        stops("\n", paste0(" - ", errors, collapse = "\n"))
    }

    # This guarantees id will be
    # valid in almost all cases.
    id <- as.character(id %??% constant("unknown"))
    return(sprintf("['%s'] %s", id, errors))
}
