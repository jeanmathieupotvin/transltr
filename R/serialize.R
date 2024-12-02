#' Serialize Objects
#'
#' @description
#' Convert [`Translator`][Translator] objects, [`Text`][Text] objects, and
#' [`Location`][Location] objects to a [YAML](https://yaml.org/spec/1.1/)
#' string.
#'
#' Convert translations contained by a [`Translator`][Translator] object into
#' a plain text and *flat* format that is easy to use and understand.
#'
#' @details
#' The data serialization process is broken down in 2 steps: [export()],
#' and [format()].
#'
#' [export()] converts objects into *transient* objects of class
#' [`Exported`][export()]. These are trivially convertable to an equivalent
#' YAML data structure via [format()]. They are *transient* because they are
#' always formatted immediately after being created. As such, [export()] can
#' be viewed as a preprocessor, and [format()] as the *true* serializer.
#'
#' [`Exported`][export()] objects can be converted back to equivalent \R
#' objects via [import()].
#'
#' ## The Exported Base Class
#'
#' Generally speaking, an [`Exported`][export()] object is a named list of S3
#' class [`Exported`][export()] always having a `tag` attribute whose value is
#' equal to the super-class of argument `x`.
#'
#' Classes [`Translator`][Translator], [`Text`][Text], and
#' [`Location`][Location] each have a matching [`Exported`][export()]
#' super-class: [`ExportedTranslator`][export()], [`ExportedText`][export()],
#' and [`ExportedLocation`][export()]. There are three main differences between
#' both sets:
#'
#'   1. field names are written as whole (human-readable) words,
#'   2. source text is treated independently from translations, and
#'   3. unset fields are set equal to `NULL` (a `~` in YAML).
#'
#' The information is otherwise identical, albeit structured slightly
#' differently.
#'
#' ## The ExportedTranslations Super-Class
#'
#' This class restructures translations by languages. Unlike other
#' [`Exported`][export()] super-classes, it has no corresponding
#' *non-exported* class. Objects of this class are created by applying
#' [export_translations()] on a [`Translator`][Translator] object.
#'
#' Objects of this class are used to create
#' *[Portable Translations File][translations_read()]*.
#'
#' ### Format
#'
#' An [`ExportedTranslations`][export()] has a dual format. Fields `Language`,
#' `Native Language`, `Source Language`, and `Source Native Language` are
#' serialized as a YAML string. However, field `Translations` is formatted
#' separately as a *flat* string: a sequence of plain text, and unindented
#' sections. The example below highlights both formats.
#'
#' ```
#' %YAML 1.1
#' ---
#' !<Translations>
#' Language: fr
#' Native Language: Français
#' Source Language: en
#' Source Native Language: English
#'
#' [[hash]]
#'
#' The source text is inserted here. Its hash is determined automatically.
#' They both must be left as is.
#'
#' [[Translation]]
#'
#' The translation, or a placeholder equal to '<none>'. It must be removed
#' before completing the section.
#'
#' Single line breaks are interpreted as single spaces, like other popular
#' text formats. This avoids writing, and working with cumbersome long strings.
#' ```
#'
#' @param x Any \R object. A [`Translator`][Translator] object, or an
#'   [`ExportedTranslator`][export()] object for [export_translations()].
#'
#' @param set_translations A non-[NA][base::NA] logical value. Should
#'   translations be included in the resulting [`ExportedText`][export()]
#'   object? If `FALSE`, field `Translations` is set equal to `NULL`.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @returns
#' [export()] return a named list of S3 class [`Exported`][export()] and
#' super-class
#'
#'   * [`ExportedTranslator`][export()] if `x` is a [`Translator`][Translator] object,
#'   * [`ExportedText`][export()] if `x` is a [`Text`][Text] object, or
#'   * [`ExportedLocation`][export()] if `x` is a [`Location`][Location] object.
#'
#' [import()] returns
#'
#'   * a [`Translator`][Translator] object if `x` is an [`ExportedTranslator`][export()] object,
#'   * a [`Text`][Text] object if `x` is an [`ExportedText`][export()] object,
#'   * a [`Location`][Location] object if `x` an [`ExportedLocation`][export()] object, or
#'   * the value passed to `x` as is otherwise (the default method).
#'
#' [export_translations()] returns a named list containing further named lists
#' of S3 class [`ExportedTranslations`][export()] and of length 5. Each one
#' contains the following elements.
#'
#' \describe{
#'   \item{`Language`}{A non-empty and non-[NA][base::NA] character string.
#'     The underlying translations' target language.}
#'   \item{`Native Language`}{A non-empty and non-[NA][base::NA] character
#'     string. The full native language name of the target language.}
#'   \item{`Source Language`}{A non-empty and non-[NA][base::NA] character
#'     string. The underlying source code's language.}
#'   \item{`Source Native Language`}{A non-empty and non-[NA][base::NA]
#'     character string. The full native language name of the source language.}
#'   \item{`Translations`}{A named list containing further named lists. Each
#'     sublist contains two values:
#'     \describe{
#'       \item{`Source Text`}{A non-empty and non-[NA][base::NA] character
#'         string.}
#'       \item{`Translation`}{A non-empty and non-[NA][base::NA] character string,
#'         or `NULL` if unavailable.}
#'     }
#'     Their names represent (reduced) hashes extracted from `x`.}
#' }
#'
#' [format()] returns a named character vector. Its length and contents depend
#' on the underlying method.
#'
#'   * [format.Exported()] returns a character string, a YAML serialization `x`.
#'   * [format.ExportedTranslator()] returns a character vector of length 2 and
#'     containing the following elements.
#'
#'     \describe{
#'       \item{`instructions`}{A character string. A set of YAML comments.}
#'       \item{`translator`}{A character string. The YAML serialization of `x`.}
#'     }
#'
#'   * [format.ExportedTranslations()] returns a character vector of length 3
#'     containing the following elements.
#'
#'     \describe{
#'       \item{`instructions`}{A character string. A set of YAML comments.}
#'       \item{`languages`}{A character string. A YAML collection containing
#'         basic information on source and target languages.}
#'       \item{`translations`}{A character string. Pairs of source text and
#'         translations formatted as *flat* sections. See above.}
#'     }
#'
#' [print()] returns argument `x`, invisibly.
#'
#' @note
#' The rationale behind [export.default()] is that any \R type, or class that
#' does not have a dedicated [import()] method should be considered *imported*
#' already. This is useful when [applying][apply()] [import()] on many objects
#' having different classes.
#'
#' @seealso
#' [Official YAML 1.1 specification](https://yaml.org/spec/1.1/),
#' [translator_read()],
#' [translator_write()],
#' [translations_read()],
#' [translations_write()]
#'
#' @aliases
#' Exported
#' ExportedTranslator
#' ExportedText
#' ExportedLocation
#' ExportedTranslations
#'
#' @name serialize
#' @rdname serialize
#' @keywords internal
export <- function(x, ...) {
    UseMethod("export")
}

#' @rdname serialize
#' @keywords internal
import <- function(x, ...) {
    UseMethod("import")
}

#' @rdname serialize
#' @keywords internal
export_translations <- function(x = translator(), ...) {
    if (is_translator(x)) {
        x <- export(x)
    }
    if (!inherits(x, "ExportedTranslator") ||
        !identical(attr(x, "tag"), "Translator")) {
        stops("'x' must be a 'Translator' object, or a 'ExportedTranslator' object.")
    }

    source_lang      <- x$`Source Language`
    source_native    <- x$Languages[[x$`Source Language`]]
    non_source_langs <- names(x$Languages)[names(x$Languages) != source_lang]

    # For each non-NULL entry in Translations Files
    # (for each non-source language), extract basic
    # information on target and source languages,
    # and all available translations.
    out <- lapply(non_source_langs, \(lang) {
        out <- list(
            Language                 = lang,
            `Native Language`        = x$Languages[[lang]],
            `Source Language`        = source_lang,
            `Source Native Language` = source_native,
            Translations             = lapply(.get_texts(x), \(txt) {
                return(
                    list(
                        `Source Text` = txt$`Source Text`,
                        Translation   = txt$`Translations`[[lang]]))
        }))

        return(
            structure(out,
                tag   = "Translations",
                class = c("ExportedTranslations", "Exported")))
    })

    names(out) <- non_source_langs
    return(out)
}

#' @rdname serialize
#' @keywords internal
#' @export
export.Translator <- function(x, ...) {
    if (length(source_lang <- x$source_langs) > 1L) {
        stops("all 'Text' objects of 'x' must have the same 'source_lang'.")
    }

    texts <- lapply(x$hashes, \(hash) {
        return(export(x$get_text(hash), ...))
    })

    # Setting names of langs also sets
    # names of `Translations Files` below.
    names(langs) <- langs <- names(x$native_languages)

    out <- list(
        Version              = 1L,
        `Generated By`       = constant("generated-by"),
        `Generated On`       = utc(),
        Identifier           = x$id,
        `Hashing Algorithm`  = x$hash_algorithm,
        `Source Language`    = source_lang,
        Languages            = as.list(x$native_languages),
        `Translations Files` = lapply(langs, \(lang) {
            # Since translating source text to itself
            # makes no sense, the underlying entry is
            # left as NULL.
            return(if (lang != source_lang) sprintf("%s.txt", lang))
        }))

    return(
        structure(c(out, texts),
            tag   = "Translator",
            class = c("ExportedTranslator", "Exported")))
}

#' @rdname serialize
#' @keywords internal
#' @export
export.Text <- function(x, set_translations = FALSE, ...) {
    assert_lgl1(set_translations)

    source_lang_is_set <- x$source_lang != constant("unset")

    out <- list(
        `Hashing Algorithm` = x$hash_algorithm,
        Hash                = if (source_lang_is_set) x$hash,
        `Source Language`   = if (source_lang_is_set) x$source_lang,
        `Source Text`       = if (source_lang_is_set) str_wrap(x$source_text, 80L),
        Translations        = NULL,
        Locations           = lapply(x$locations, export, ...))

    if (set_translations) {
        trans <- x$translations
        trans <- trans[names(trans) != x$source_lang %??% ""]
        out$Translations <- lapply(trans, str_wrap, width = 80L)
    }

    names(out$Locations) <- NULL
    return(
        structure(out,
            tag   = "Text",
            class = c("ExportedText", "Exported")))
}

#' @rdname serialize
#' @keywords internal
#' @export
export.Location <- function(x, ...) {
    out <- list(Path = x$path, Ranges = .location_format_range(x, ...))
    return(
        structure(out,
            tag   = "Location",
            class = c("ExportedLocation", "Exported")))
}

#' @rdname serialize
#' @export
import.ExportedTranslator <- function(x, ...) {
    trans <- Translator$new(x$Identifier, x$`Hashing Algorithm`)
    texts <- lapply(.get_texts(x), import, ...)

    do.call(trans$set_texts, texts)
    do.call(trans$set_native_languages, x$Languages)

    attr(trans, "translations_files") <- x$`Translations Files`
    return(trans)
}

#' @rdname serialize
#' @export
import.ExportedText <- function(x, ...) {
    # `[[` must be used here to prevent partial matching to
    # `Hashing Algorithm` in the unlikely case that Hash is
    # missing. This could happen if users manually edited a
    # PTF. The label is used when throwing conditions below.
    label <- sprintf("Text '%s': ", x[["Hash"]] %??% constant("unknown"))

    txt <- Text$new(x$`Hashing Algorithm`)
    do.call(txt$set_locations, lapply(x$Locations, import, ...))
    do.call(txt$set_translations, x$Translations %??% list())

    if (!is.null(source_lang <- x$`Source Language`)) {
        if (is.null(x$`Source Text`)) {
            warning(call. = FALSE,
                label,
                "'Source Language' is defined, but not 'Source Text'.",
                " This is likely the result of a manual edit.")
        }

        txt$set_translation(source_lang, text_normalize(x$`Source Text`))
        txt$source_lang <- source_lang
    }

    if ((x[["Hash"]] %??% constant("unset")) != txt$hash) {
        warning(call. = FALSE,
            label,
            sprintf("'Hash' is not equal to expected value ('%s').", txt$hash),
            " The latter will be used.")
    }

    return(txt)
}

#' @rdname serialize
#' @export
import.ExportedLocation <- function(x, ...) {
    # Extract digits from ranges, construct
    # character vectors of numbers, and try
    # converting them to integers. This works
    # with all supported formats.
    ranges <- x$Ranges
    chrs   <- regmatches(ranges, gregexpr("[0123456789]+", ranges))
    ints   <- suppressWarnings(lapply(chrs, as.integer))

    # Validate resulting integer vectors.
    # There should be one for each range.
    # The callback returns nothing.
    map(ints = ints, index = seq_along(ints), fun = \(ints, index) {
        if (length(ints) != 4L || anyNA(ints) || !all(ints > 0L)) {
            stopf("could not convert range '%s'.", ranges[[index]])
        }
    })

    return(
        location(x$Path,
            vapply_1i(ints, `[[`, 1L),
            vapply_1i(ints, `[[`, 2L),
            vapply_1i(ints, `[[`, 3L),
            vapply_1i(ints, `[[`, 4L)))
}

#' @rdname serialize
#' @export
import.default <- function(x, ...) {
    return(x)
}

#' @rdname serialize
#' @export
format.Exported <- function(x, ...) {
    return(
        yaml::as.yaml(x,
            line.sep = "\n",
            indent   = 2L,
            unicode  = TRUE,
            indent.mapping.sequence = TRUE))
}

#' @rdname serialize
#' @export
format.ExportedTranslator <- function(x, ...) {
    instructions <- paste0(
        "# Portable Translator File\n",
        "#\n",
        "# Instructions:\n",
        "#  - You may edit Identifier, Languages, and Translations Files.\n",
        "#  - Do not edit other entries by hand; edit source scripts instead.\n",
        "#  - Translations are stored in distinct Portable Translations Files.\n",
        "%YAML 1.1\n",
        "---")

    return(c(
        instructions = instructions,
        translator   = NextMethod(x)))
}

#' @rdname serialize
#' @export
format.ExportedTranslations <- function(x, ...) {
    # NOTE: this function implements the flat format.
    # It is tightly coupled with translations_read().

    placeholder  <- constant("placeholder")
    instructions <- paste0(
        "# Portable Translations File\n",
        "#\n",
        "# Instructions:\n",
        "#  - Edit each [[Translation]] section below by hand.\n",
        "#  - Translate what is in each preceding [[id]] section.\n",
        sprintf("#  - Remove any %s placeholder before.\n", placeholder),
        "#  - Use any text editor to do so.\n",
        "#  - Use encoding UTF-8 whenever you are prompted to choose.\n",
        "#  - Do not edit other sections.\n",
        "#  - You may split long sentences with new lines.\n",
        "%YAML 1.1\n",
        "---")

    # Create flat sections.
    # NULL values are substituted
    # by a semantic placeholder.
    trans <- x$Translations
    flat  <- paste0(collapse = "\n\n", sprintf(
        "[[%s]]\n\n%s\n\n[[Translation]]\n\n%s",
        names(trans),
        lapply(trans, \(x) x$`Source Text` %??% placeholder),
        lapply(trans, \(x) x$Translation   %??% placeholder)))

    # Translations are removed before passing
    # x to next format() method because they
    # are included as flat sections already.
    x$Translations <- NULL
    return(c(
        instructions = instructions,
        languages    = NextMethod(x),
        translations = flat))

}

#' @rdname serialize
#' @export
print.Exported <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    return(invisible(x))
}


# Internal functions -----------------------------------------------------------


.get_texts <- function(x = export(translator())) {
    return(x[vapply_1l(x, \(x) identical(attr(x, "tag"), "Text"))])
}
