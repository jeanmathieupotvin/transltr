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
#' [Exported Translator files][translator_write()], and
#' [Exported Translations files][translations_read()].
#'
#' While [serialize()] and [serialize_translations()] are distinct, they share
#' a common *design*, and perform the same *thing*, at least conceptually. The
#' same is true for [deserialize()], and [deserialize_translations()].
#'
#' ## Serialization
#'
#' The data serialization process performed by [serialize()] and
#' [serialize_translations()] is internally broken down into 2 steps: objects
#' are first *exported* before being *serialized*.
#'
#' [export()] and [export_translations()] are *preserializers* that convert
#' objects into *transient* objects of class [`Exported*`][export()]. Their
#' sole purpose is to ease the conversion of environments to textual
#' representations ([`Translator`][Translator] objects and [`Text`][Text]
#' objects are stored as environments internally). They are never returned to
#' the user: [serialize()] and [serialize_translations()] immediately transform
#' them into suitable character strings. The latter outputs a
#' [FLAT object][flat_serialize()], and the former, a
#' [YAML object][yaml::as.yaml()].
#'
#' ## Deserialization
#'
#' The data deserialization process performed by [deserialize()] and
#' [deserialize_translations()] is internally broken down into 3 steps: objects
#' are first *deserialized*, then *validated*, and finally, *imported*.
#'
#' [deserialize()], and [deserialize_translations()] first deserializes `string`
#' into an \R named list that is **presumed** to be an object of class
#' [`Exported*`][export()]. The former relies on underlying
#' [YAML tags](https://yaml.org/spec/1.1/#id858600) to make such assumptions.
#' The contents of the resulting *transient* object is then thoroughly checked
#' with [validate()], which dispatches on the object's **presumed** class.
#' Finally, a valid object is *imported* back into an appropriate \R object
#' via [import()].
#'
#' ## `Exported` Classes
#'
#' [`Exported*`][export()] may refer to classes
#' [`ExportedTranslator`][export()],
#' [`ExportedText`][export()],
#' [`ExportedLocation`][export()], and
#' [`ExportedTranslations`][export()].
#'
#' Generally speaking, an [`Exported*`][export()] object is a named list of S3
#' class [`Exported*`][export()] always having a `tag` attribute whose value is
#' equal to the super-class of argument `x`. The exact class depends on `x`.
#' [`Exported*`][export()] objects can be converted back to equivalent \R
#' objects via [import()].
#'
#' There are three main differences between an object and its *exported*
#' counterpart.
#'
#'   1. field names are written as whole (human-readable) words,
#'   2. source text is treated independently from translations, and
#'   3. unset fields are set equal to `NULL` (a `~` in YAML).
#'
#' The information is otherwise identical, albeit structured differently for
#' presentational purposes.
#'
#' ## The `ExportedTranslations` Super-class
#'
#' [export_translations()] is different because it returns an object of S3 class
#' [`ExportedTranslations`][export()] which has no *unexported* counterpart.
#' Its purpose is to restructure translations extracted from a
#' [`Translator`][Translator] object by language. It is used to create
#' [Exported Translations file][translations_read()].
#'
#' [`ExportedTranslations`][export()] objects are created from a
#' [`Translator`][Translator] object. The value passed to `lang` must have a
#' corresponding registered native language name. See
#' [`Translator$native_languages`][Translator] for more information.
#'
#' Unavailable translations are automatically replaced by a placeholder.
#'
#' @param x Any \R object.
#'
#' @param tr A [`Translator`][Translator] object.
#'
#'   This argument is **optional** for [deserialize_translations()], and
#'   [import.ExportedTranslations()]. It allows a [`Translator`][Translator]
#'   object to register imported translations as long as they correspond to
#'   an existing source text (a registered [`Text`][Text] object).
#'
#' @param string A non-empty and non-[NA][base::NA] character string. Contents
#'   to deserialize.
#'
#' @param parent_dir An **optional** non-[NA][base::NA] character string. It
#'   can be empty. The parent directory to use when creating entries of field
#'   `Translations Files` of an [`ExportedTranslator`][export()] object. See
#'   [translator_write()] for more information. Empty values are converted to
#'   the root location (`/`) by [file.path()].
#'
#' @param set_uuid A non-[NA][base::NA] logical value. Should a `_Uuid` field
#'   uniquely identifying outputs of [export()] be included? If `TRUE`, it is
#'   set equal to [uuid()]. This field is required by [validate()] to throw
#'   meaningful error messages­.
#'
#' @param set_translations A non-[NA][base::NA] logical value. Should
#'   translations be included in the resulting [`ExportedText`][export()]
#'   object? If `FALSE`, field `Translations` is set equal to `NULL`.
#'
#' @template param-lang
#'
#' @param ... Further arguments passed to, or from other methods.
#'
#' @returns
#' [serialize()] and [serialize_translations()] return a character string:
#' a [YAML](https://yaml.org/spec/1.1/) string, and a [FLAT][flat_serialize()]
#' string, respectively.
#'
#' [export()] returns a named list of S3 class
#'
#'   * [`ExportedTranslator`][export()] if `x` is a [`Translator`][Translator]
#'     object,
#'   * [`ExportedText`][export()] if `x` is a [`Text`][Text] object, or
#'   * [`ExportedLocation`][export()] if `x` is a [`Location`][Location] object.
#'
#' See Details above for more information.
#'
#' [export_translations()] returns a named list of S3 class
#' [`ExportedTranslations`][export()] containing the following elements.
#'
#' \describe{
#'   \item{`_Uuid`}{A non-empty and non-[NA][base::NA] character
#'     string if `set_uuid` is `TRUE`. A
#'     [universally unique identifier](https://en.wikipedia.org/wiki/Universally_unique_identifier)
#'     used to throw suitable error messages. It should be ignored by users.}
#'   \item{`Identifier`}{The unique identifier of `x`. See
#'     [`Translator$id`][Translator] for more information.}
#'   \item{`Language Code`}{The value of `lang`.}
#'   \item{`Language`}{The target (native) language. See
#'     [`Translator$native_languages`][Translator] for more information.}
#'   \item{`Source Language`}{The source language. See
#'     [`Translator$source_langs`][Translator] for more information.}
#'   \item{`Translations`}{A named list containing further named lists. Each
#'     sublist contains two values:
#'     \describe{
#'       \item{`Source Text`}{A non-empty and non-[NA][base::NA] character
#'         string.}
#'       \item{`Translation`}{A non-empty and non-[NA][base::NA] character
#'         string, or `NULL` if unavailable.}
#'     }
#'     See [`Text$translations`][Text] for more information. Their names are
#'     (reduced) hashes extracted from `x`.}
#' }
#'
#' [deserialize()] and [import()] returns
#'
#'   * a [`Translator`][Translator] object if `x` is an
#'     [`ExportedTranslator`][export()] object,
#'   * a [`Text`][Text] object if `x` is an [`ExportedText`][export()] object,
#'   * a [`Location`][Location] object if `x` an [`ExportedLocation`][export()]
#'     object.
#'
#' [import.ExportedTranslator()] further sets a `translations_files` attribute
#' holding the cached value of `x$Translations Files`.
#'
#' [deserialize_translations()] and [import.ExportedTranslations()] return an
#' [`ExportedTranslations`][export()] object. Its contents is reformatted, if
#' necessary.
#'
#'   * [import.ExportedTranslations()] further registers imported translations
#'     if a [`Translator`][Translator] object is passed to itself, or to
#'     [deserialize_translations()]. Translations must correspond to an existing
#'     source text (a registered [`Text`][Text] object). Otherwise, they are
#'     skipped silently. The [`Translator`][Translator] object is updated
#'     **by reference** and is not returned ([`Translator`][Translator] objects
#'     are [environments]).
#'
#' [import.default()] is used for its side-effect of throwing an error for
#' unsupported objects.
#'
#' [validate()] returns `x` if it is valid and throws an error otherwise. It
#' is designed to fail fast: error messages are **not accumulated**. Classes
#' (and underlying objects) that does not have a [validate()] method are
#' considered to be valid by default.
#'
#' Therefore, [validate.default()] simply returns `x` like [identity()] would.
#'
#' [translations_files()] returns a named list.
#'
#'   * If `tr` has a valid `translations_files` attribute (a non-empty named
#'     list), it is returned. See [import.ExportedTranslator()] for more
#'     information.
#'   * Otherwise, a named list of file paths is constructed from
#'     `tr$native_languages` (the source language is ignored).
#'     See [`Translator$native_languages`][Translator] for more information.
#'
#' [get_uuid()] safely extracts field `_Uuid` from `x`, and returns it if the
#' underlying value is a non-empty and non-[NA][base::NA] character string.
#' Otherwise, a default value is returned.
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
#' @examples
#' # Set source language.
#' language_source_set("en")
#'
#' # Create Location, Text, and Translator objects.
#' loc <- location("file-a", 1L, 2L, 3L, 4L)
#'
#' txt <- text(
#'   location("a", 1L, 2L, 3L, 4L),
#'   en = "Hello, world!",
#'   fr = "Bonjour, monde!",
#'   es = "¡Hola, mundo!")
#'
#' tr <- translator(
#'   id = "test-translator",
#'   en = "English",
#'   es = "Español",
#'   fr = "Français",
#'   text(
#'     location("a", 1L, 2L, 3L, 4L),
#'     en = "Hello, world!",
#'     fr = "Bonjour, monde!"),
#'   text(
#'     location("b", 1L, 2L, 3L, 4L),
#'     en = "Farewell, world!",
#'     fr = "Au revoir, monde!"))
#'
#' # Serialize objects. This is what translator_write(),
#' # and translations_write() call internally.
#' serialize(loc)
#' serialize(txt)
#' serialize(tr)
#' serialize_translations(tr, "fr")
#' serialize_translations(tr, "es")
#'
#' # serialize(), and serialize_translations() both call export() internally.
#' # Field `_Uuid` can be omitted by setting set_uuid equal to `FALSE`.
#' export(loc, set_uuid = FALSE)
#' export(txt)
#' export(tr)
#' export_translations(tr, "fr")
#' export_translations(tr, "es", set_uuid = FALSE)
#'
#' # Translations can be exported along with original objects.
#' # This is not the default behavior of the package.
#' export(txt, set_translations = TRUE)
#' export(tr,  set_translations = TRUE)
#'
#' # Deserialize objects. This is what translator_read(), and
#' # translations_read() call internally.
#' deserialize(serialize(loc))
#' deserialize(serialize(txt))
#' deserialize(serialize(tr))
#' deserialize_translations(serialize_translations(tr, "fr"))
#' deserialize_translations(serialize_translations(tr, "es"))
#'
#' # deserialize(), and deserialize_translations() both call validate()
#' # internally. validate() checks that the underlying input can safely be
#' # passed to import(). What deserialization functions internally passes to
#' # validate() are candidate 'Exported' objects that may, or may not be valid.
#' validate(export(loc))
#' validate(export(txt))
#' validate(export(tr))
#' validate(export_translations(tr, "fr"))
#'
#' # Introduce an error in an ExportedLocation object for illustration purposes.
#' # validate() uses field `_Uuid` to throw meaningful error messages.
#' loc_err <- export(loc)
#' loc_err$Path <- 1L
#'
#' \dontrun{validate(loc_err)}
#'
#' # deserialize(), and deserialize_translations() both call import() on valid
#' # Exported* objects. Any `_Uuid` field is discarded by import().
#' import(export(loc))
#' import(export(txt))
#' import(export(tr))
#' import(export_translations(tr, "fr"))
#' import(export_translations(tr, "es"))
#'
#' # import() is the conceptual inverse of export(). However, this is not
#' # quite true for import.ExportedTranslations(), because it replaces any
#' # previous placeholders stemming from export_translations() with another
#' # placeholder (for internal consistency).
#' cat(serialize_translations(tr, "es"), "\n")
#' deserialize_translations(serialize_translations(tr, "es"))
#'
#' # A Translator object may be passed to deserialize_translations(), and
#' # import.ExportedTranslations(). It accumulates translations, and is
#' # modified in place (it is not returned). Note that translations are
#' # registered only if they correspond to an existing source text, and
#' # are skipped otherwise.
#' tr_new <- Translator$new(id = "test-translator")
#' tr_new$set_text(en = "Hello, world!")
#' tr_new$set_text(en = "Farewell, world!")
#' deserialize_translations(serialize_translations(tr, "fr")) , tr_new)
#'
#' # translations_files() is used to create the 'Translations Files' field
#' # of an ExportedTranslator object.
#' translations_files(tr)
#' translations_files(tr, getwd())
#'
#' # It is also used to fetch internal attribute `translations_files` set by
#' # import.ExportedTranslator().
#' translations_files(import(export(tr)))
#'
#' get_uuid(export(loc))  ## Outputs the current uuid
#' get_uuid(1L)           ## Outputs "<unkwnown>"
#'
#' @rdname serialize
#' @keywords internal
serialize <- function(x, ...) {
    return(
        yaml::as.yaml(
            export(x, ...),
            line.sep = "\n",
            indent   = 2L,
            unicode  = TRUE,
            indent.mapping.sequence = TRUE))
}

#' @rdname serialize
#' @keywords internal
serialize_translations <- function(tr = translator(), lang = "", set_uuid = TRUE) {
    return(flat_serialize(export_translations(tr, lang, set_uuid)))
}

#' @rdname serialize
#' @keywords internal
deserialize <- function(string = "") {
    assert_chr1(string)

    obj <- tryCatch({
        yaml::yaml.load(string,
            # eval.expr is disallowed for security.
            eval.expr      = FALSE,
            as.named.list  = TRUE,
            merge.warning  = TRUE,
            # The YAML engine identifies classes from
            # tags. import() does the rest, see below.
            handlers = list(
                Translator = \(x) structure(x, class = "ExportedTranslator"),
                Text       = \(x) structure(x, class = "ExportedText"),
                Location   = \(x) structure(x, class = "ExportedLocation")))
    },
    condition = \(cond) {
        stopf(
            "while unserializing object: %s",
            # Below we attempt to formulate a coherent
            # sentence from error messages returned by
            # the YAML parser. This is not perfect, but
            # better than nothing.
            gsub("[ \n\t.]*$", ".", tolower(cond$message)))
    })

    return(import(validate(obj)))
}

#' @rdname serialize
#' @keywords internal
deserialize_translations <- function(string = "", tr) {
    obj <- structure(flat_deserialize(string), class = "ExportedTranslations")
    return(import(validate(obj), tr))
}

#' @rdname serialize
#' @keywords internal
export <- function(x, ...) {
    UseMethod("export")
}

#' @rdname serialize
#' @keywords internal
export_translations <- function(tr = translator(), lang = "", set_uuid = TRUE) {
    assert_chr1(lang)
    assert_lgl1(set_uuid)

    if (!is_translator(tr)) {
        stops("'tr' must be a 'Translator' object.")
    }
    if (length(source_lang <- tr$source_langs) > 1L) {
        stops("all 'Text' objects of 'tr' must have the same 'source_lang'.")
    }
    if (!is_match(lang, names(native_languages <- tr$native_languages))) {
        stops("'lang' must have a corresponding native language registered in 'tr'.")
    }

    placeholder  <- constant("untranslated")
    translations <- lapply(lapply(tr$hashes, tr$get_text), \(txt) {
        return(
            list(
                `Source Text` = txt$source_text %??% placeholder,
                Translation   = txt$get_translation(lang) %??% placeholder))
    })

    out <- list(
        `_Uuid`           = NULL,
        Identifier        = tr$id,
        `Language Code`   = lang,
        Language          = native_languages[[lang]],
        `Source Language` = native_languages[[source_lang]],
        Translations      = translations)

    out$`_Uuid` <- if (set_uuid) uuid() else NULL
    return(structure(out, class = "ExportedTranslations", tag = "Translations"))
}

#' @rdname serialize
#' @keywords internal
import <- function(x, ...) {
    UseMethod("import")
}

#' @rdname serialize
#' @keywords internal
validate <- function(x, ...) {
    UseMethod("validate")
}

#' @rdname serialize
#' @keywords internal
#' @export
export.Translator <- function(x, set_uuid = TRUE, parent_dir, ...) {
    assert_lgl1(set_uuid)

    # It validates x, and parent_dir.
    files <- translations_files(x, parent_dir)

    out <- list(
        `_Uuid`              = NULL,
        Identifier           = x$id,
        `Hashing Algorithm`  = x$hash_algorithm,
        `Source Language`    = x$source_langs,
        Languages            = as.list(x$native_languages),
        `Translations Files` = files,
        `Texts`              = lapply(x$hashes, \(hash) {
            export(x$get_text(hash), set_uuid, ...)
        }))

    out$`_Uuid` <- if (set_uuid) uuid() else NULL
    return(structure(out, class = "ExportedTranslator", tag = "Translator"))
}

#' @rdname serialize
#' @keywords internal
#' @export
export.Text <- function(x, set_uuid = TRUE, set_translations = FALSE, ...) {
    assert_lgl1(set_uuid)
    assert_lgl1(set_translations)

    is_set <- x$source_lang != constant("unset")

    out <- list(
        `_Uuid`             = NULL,
        `Hashing Algorithm` = x$hash_algorithm,
        Hash                = if (is_set) x$hash,
        `Source Language`   = if (is_set) x$source_lang,
        `Source Text`       = if (is_set) strwrap(x$source_text, 80L),
        Translations        = if (set_translations) {
            lapply(
                x$translations[names(x$translations) != x$source_lang],
                strwrap,
                width = 80L)
        },
        Locations = unname(lapply(x$locations, export, set_uuid, ...)))

    out$`_Uuid` <- if (set_uuid) uuid() else NULL
    return(structure(out, class = "ExportedText", tag = "Text"))
}

#' @rdname serialize
#' @keywords internal
#' @export
export.Location <- function(x, set_uuid = TRUE, ...) {
    assert_lgl1(set_uuid)

    out <- list(
        `_Uuid` = NULL,
        Path    = x$path,
        Ranges  = .location_format_range(x, ...))

    out$`_Uuid` <- if (set_uuid) uuid() else NULL
    return(structure(out, class = "ExportedLocation", tag = "Location"))
}

#' @rdname serialize
#' @keywords internal
#' @export
import.ExportedTranslator <- function(x, ...) {
    tr <- Translator$new(x[["Identifier"]], x[["Hashing Algorithm"]])

    do.call(tr$set_native_languages, x[["Languages"]])
    do.call(tr$set_texts, lapply(x[["Texts"]], import, ...))

    attr(tr, "translations_files") <- x[["Translations Files"]]
    return(tr)
}

#' @rdname serialize
#' @keywords internal
#' @export
import.ExportedText  <- function(x, ...) {
    txt <- Text$new(x[["Hashing Algorithm"]])

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
        warning(call. = FALSE,
            sprintf(
                "in 'Text' '%s': 'Hash' is not equal to expected hash ('%s').",
                get_uuid(x),
                txt$hash),
            " The latter will be used.")
    }

    return(txt)
}

#' @rdname serialize
#' @keywords internal
#' @export
import.ExportedLocation <- function(x, ...) {
    # Extract digits from ranges, construct
    # character vectors of numbers, and try
    # converting them to integers. This works
    # with all supported formats.
    ranges <- x[["Ranges"]]
    chrs   <- regmatches(ranges, gregexpr("[0123456789]+", ranges))
    ints   <- suppressWarnings(lapply(chrs, as.integer))

    # Validate resulting integer vectors.
    # There should be one for each range.
    # The callback returns nothing.
    map(ints = ints, index = seq_along(ints), fun = \(ints, index) {
        if (length(ints) != 4L || anyNA(ints) || !all(ints > 0L)) {
            stopf(
                "in 'Location' '%s': the following range cannot be converted: '%s'.",
                get_uuid(x),
                ranges[[index]])
        }
    })

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
import.ExportedTranslations <- function(x, tr, ...) {
    x[["Translations"]] <- lapply(x[["Translations"]], \(subsection) {
        placeholder <- constant("empty")

        # Try extracting child elements. If either is NULL,
        # set them equal to a placeholder. This happens if
        # a section is totally missing.
        source_text <- subsection[["Source Text"]] %??% placeholder
        translation <- subsection[["Translation"]] %??% placeholder

        # If either is an empty string, replace them
        # by the empty constant. A field may be empty
        # when a section is lacking contents.
        source_text <- if (nzchar(source_text)) source_text else placeholder
        translation <- if (nzchar(translation)) translation else placeholder

        # If the underlying translation field is equal
        # to the default untranslated constant, replace
        # it by the empty constant. The untranslated
        # constant is added by export_translations().
        translation <- if (identical(translation, constant("untranslated"))) {
            placeholder
        } else {
            translation
        }

        return(
            list(
                `Source Text` = normalize(source_text),
                Translation   = normalize(translation)))
    })

    # Translator and Text objects are environments. They
    # have reference semantics and are updated in place.
    if (!missing(tr)) {
        if (!is_translator(tr)) {
            stops("'tr' must be a 'Translator' object.")
        }
        if (!identical(x[["Identifier"]], tr$id)) {
            stopf(
                "'Identifier' ('%s') of translations does not match expected 'Translator$id' ('%s').",
                x[["Identifier"]], tr$id)
        }

        lang <- x[["Language Code"]]
        do.call(tr$set_native_languages, structure(x["Language"], names = lang))

        # Hashes are passed as names.
        translations <- lapply(x[["Translations"]], `[[`, i = "Translation")
        hashes       <- names(translations)

        # Register translations.
        map(hashes, translations, fun = \(hash, translation) {
            if (!is.null(txt <- tr$get_text(hash))) {
                txt$set_translation(lang, translation)
            }
        })
    }

    return(x)
}

#' @rdname serialize
#' @keywords internal
import.default <- function(x, ...) {
    stops(
        "deserialized object is not supported by 'transltr'.",
        " The underlying serialization is likely missing a '!<type>' tag.")
}

#' @rdname serialize
#' @keywords internal
#' @export
validate.ExportedTranslator <- function(x, ...) {
    langs <- x[["Languages"]]
    files <- x[["Translations Files"]]
    texts <- x[["Texts"]]

    if (!is_chr1(x[["Identifier"]])) {
        stopf(
            "in 'Translator' '%s': 'Identifier' must be a YAML scalar parsed as a non-empty R character string.",
            get_uuid(x))
    }
    if (!is_match(x[["Hashing Algorithm"]], hash_algorithms())) {
        stopf(
            "in 'Translator' '%s': 'Hashing Algorithm' must be a YAML scalar equal to %s.",
            get_uuid(x),
            to_string(hash_algorithms(), TRUE))
    }
    if (!is_chr1(x[["Source Language"]])) {
        stopf(
            "in 'Translator' '%s': 'Source Language' must be a YAML scalar parsed as a non-empty R character string.",
            get_uuid(x))
    }
    if (!is_list(langs, TRUE) || !is_named(langs)) {
        stopf(
            "in 'Translator' '%s': 'Languages' must a YAML mapping.",
            get_uuid(x))
    }
    if (!all(vapply_1l(langs, is_chr1))) {
        stopf(
            "in 'Translator' '%s': entries of 'Languages' must all be YAML scalars parsed as a non-empty R character strings.",
            get_uuid(x))
    }
    if (!is_list(files, TRUE) || !is_named(files)) {
        stopf(
            "in 'Translator' '%s': 'Translations Files' must a YAML mapping.",
            get_uuid(x))
    }
    if (!all(vapply_1l(files, is_chr1))) {
        stopf(
            "in 'Translator' '%s': entries of 'Translations Files' must all be YAML scalars parsed as non-empty R character strings.",
            get_uuid(x))
    }
    if (!is_list(texts, TRUE) || !is_named(texts)) {
        stopf(
            "in 'Translator' '%s': 'Texts' must a YAML mapping.",
            get_uuid(x))
    }
    if (!all(vapply_1l(texts, inherits, what = "ExportedText"))) {
        stopf(
            "in 'Translator' '%s': entries of 'Texts' must all be 'Text' objects.",
            get_uuid(x))
    }

    # Remove source_lang from languages that
    # require an Exported Translations file.
    langs <- names(langs)[names(langs) != x[["Source Language"]]]

    if (!setequal(names(files), langs)) {
        stopf(
            "in 'Translator' '%s': at least one entry in 'Translations Files' is missing a corresponding entry in 'Languages', or vice-versa.",
            get_uuid(x))
    }

    return(x)
}

#' @rdname serialize
#' @keywords internal
#' @export
validate.ExportedText <- function(x, ...) {
    hash         <- x[["Hash"]]
    source_lang  <- x[["Source Language"]]
    source_text  <- x[["Source Text"]]
    translations <- x[["Translations"]]

    if (!is.null(hash) && !is_chr1(hash)) {
        stopf(
            "in 'Text' '%s': 'Hash' must be a YAML null (~), or a YAML scalar parsed as a non-empty R character string.",
            get_uuid(x))
    }
    if (!is_match(x[["Hashing Algorithm"]], hash_algorithms())) {
        stopf(
            "in 'Text' '%s': 'Hashing Algorithm' must be a YAML scalar equal to %s.",
            get_uuid(x),
            to_string(hash_algorithms(), TRUE))
    }
    if (!is.null(source_lang) && !is_chr1(source_lang)) {
        stopf(
            "in 'Text' '%s': 'Source Language' must be a YAML null (~), or a YAML parsed as a non-empty R character string.",
            get_uuid(x))
    }
    if (!is.null(source_text) && !is_chr1(source_text)) {
        stopf(
            "in 'Text' '%s': 'Source Text' must be a YAML null (~), or a YAML parsed as a non-empty R character string.",
            get_uuid(x))
    }
    if (!is.null(source_lang) && is.null(source_text) ||
        !is.null(source_text) && is.null(source_lang)) {
        stopf(
            "in 'Text' '%s': 'Source Language' is defined but not 'Source Text', or vice-versa.",
            get_uuid(x))
    }
    if (!is.null(translations) &&
        !is_list(translations, TRUE) || !is_named(translations)) {
        stopf(
            "in 'Text' '%s': 'Translations' must be a YAML null (~), or a YAML mapping.",
            get_uuid(x))
    }
    if (!all(vapply_1l(translations, is_chr1))) {
        stopf(
            "in 'Text' '%s': entries of 'Translations' must all be YAML scalars parsed as non-empty R character strings.",
            get_uuid(x))
    }

    return(x)
}

#' @rdname serialize
#' @keywords internal
#' @export
validate.ExportedLocation <- function(x, ...) {
    if (!is_chr1(x[["Path"]])) {
        stopf(
            "in 'Location' '%s': 'Path' must be a YAML scalar parsed as a non-empty R character string.",
            get_uuid(x))
    }
    if (!is_chr1(x[["Ranges"]])) {
        stopf(
            "in 'Location' '%s': 'Ranges' must be a YAML sequence of scalars.",
            get_uuid(x))
    }
    if (!all(vapply_1l(x[["Ranges"]], is_chr1))) {
        stopf(
            "in 'Location' '%s': entries of the 'Ranges' sequence must all be YAML scalars parsed as non-empty R character strings.",
            get_uuid(x))
    }

    return(x)
}

#' @rdname serialize
#' @keywords internal
#' @export
validate.default <- function(x, ...) {
    return(x)
}

#' @rdname serialize
#' @keywords internal
translations_files <- function(tr = translator(), parent_dir) {
    if (!is_translator(tr)) {
        stops("'tr' must be a 'Translator' object.")
    }
    if (length(source_lang <- tr$source_langs) > 1L) {
        stops("all 'Text' objects of 'tr' must have the same 'source_lang'.")
    }
    if (!missing(parent_dir)) {
        assert_chr1(parent_dir, TRUE)
    }

    # Remove source_lang from languages that
    # require an Exported Translations file.
    langs <- names(tr$native_languages)
    langs <- langs[langs != source_lang]

    # Check attribute 'translations_files', and return
    # it if it exists. It contains cached paths stemming
    # from a previous call to import().
    if (!is.null(files <- attr(tr, "translations_files"))) {
        assert_list(files,  x_name = "Translations Files")
        assert_named(files, x_name = "Translations Files")

        # Check if the set of keys of Translations Files is
        # the same set, or a subset of the Languages keys.
        missing <- names(files)[!match(names(files), langs, 0L)]

        if (length(missing)) {
            stopf(
                "there is a mismatch between 'Translations Files' and 'Languages'. Check language(s) %s.",
                to_string(missing, TRUE, ", and"))
        }

        return(files)
    }

    files <- sprintf("%s.txt", langs)
    paths <- if (missing(parent_dir)) files else file.path(parent_dir, files)
    return(structure(as.list(paths), names = langs))
}

#' @rdname serialize
#' @keywords internal
get_uuid <- function(x) {
    default <- constant("unknown")
    id      <- tryCatch(x[["_Uuid"]], condition = \(cond) return(default))
    return(if (is_chr1(id)) id else default)
}
