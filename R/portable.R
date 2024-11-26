#' Portable Objects
#'
#' @description
#' This help page describes all **internal** mechanisms required for exporting
#' and importing [`Translator`][Translator] objects, [`Text`][Text]
#' objects, [`Location`][Location] objects, and underlying translations.
#'
#' Portable objects are intermediaries between [`transltr`][transltr] objects
#' and their corresponding YAML (textual) representations. They are trivially
#' convertable to a YAML string using [format()].
#'
#' @details
#' The [`Portable`][portable()] family is a set of preprocessors. As such,
#' they return transient objects that users never actually *see*. They exist
#' to ease bidirectional conversion processes by handling various formatting
#' operations. Its [format()] method is a wrapper to [yaml::as.yaml()] that
#' enforces certain parameters and therefore, [print()] always outputs a YAML
#' string.
#'
#' Portable objects can be converted back to their equivalent *non-portable*
#' versions by using [as_translator()], [as_text()], and/or [as_location()].
#' The appropriate methods are used internally by [translator_read()] (as
#' handlers passed to [yaml::yaml.load()]).
#'
#' ## Low-level Constructor
#'
#' [portable()] is a low-level constructor for the [`Portable`][portable()] S3
#' class. It does two things.
#'
#'   1. It sets the usual [class][class()] attribute, adding any `super`-class
#'      if necessary.
#'   2. It sets a `tag` attribute. This tag is used to identify data types
#'      in YAML representations. See details of [yaml::as.yaml()] for more
#'      information.
#'
#' ## Constructing Portable Objects
#'
#' Classes [`Translator`][Translator], [`Text`][Text], and
#' [`Location`][Location] each have a corresponding [`Portable`][portable()]
#' class. Portable objects are always constructed from their *non-portable*
#' equivalents using the appropriate [`portable_*()`][portable()] constructor.
#'
#' @param x Usage depends on the underlying function.
#'   * A [`Translator`][Translator] object for [portable_translator()], and
#'     [portable_translations()]. All underlying [`Text`][Text] objects must
#'     have a common source language.
#'   * A [`Text`][Text] object for [portable_text()].
#'   * A [`Location`][Location] object for [portable_location()].
#'   * Any \R object for all other functions, and methods.
#'
#' @param super A character vector of non-empty and non-[NA][base::NA] values.
#'   Super-classes to append to the [class][class()] vector of `x` along with
#'   class `Portable`, if any. It may be empty.
#'
#' @param tag A `NULL`, or a non-empty and non-[NA][base::NA] character string.
#'   An optional identifier for the underlying data type. See Details below.
#'
#' @param set_translations A non-[NA][base::NA] logical value. Should
#'   translations be included in the resulting [`PortableText`][portable()]
#'   object? If `FALSE`, field `translations` is set equal to `NULL`.
#'
#' @param set_instructions A non-[NA][base::NA] logical value. Should
#'   instructions be included in the output? Instructions are included as a
#'   single string containing YAML comments . These are useful when exporting
#'   the underlying object.
#'
#' @param set_translations_files A non-[NA][base::NA] logical value. Should
#'   field `translations_files` be extracted from `x` and attached to the
#'   resulting [`Translator`][Translator] object? If `TRUE`, it is attached
#'   as an attribute named `translations_files`.
#'
#' @param how A character string equal to `"yaml"`, or `"flat"`. How to format
#'   translations contained by a [`PortableTranslations`][portable()] object.
#'   See section Class PortableTranslations below for more information.
#'
#' @param placeholder A non-empty and non-[NA][base::NA] character string.
#'   A standard string that (textually) represents unavailable translations.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @template param-lang
#'
#' @returns
#' [portable()] returns a S3 object of class [`Portable`][portable()]. Aside
#' from its updated class vector and a new `tag` attrribute, it is otherwise
#' identical to `x`. See above for more information.
#'
#' [portable_translator()] returns an S3 object of class
#' [`PortableTranslator`][portable()].
#'
#' [portable_text()] returns an S3 object of class
#' [`PortableText`][portable()].
#'
#' [portable_location()] returns an S3 object of class
#' [`PortableLocation`][portable()].
#'
#' [portable_translations()] returns an S3 object of class
#' [`PortableTranslations`][portable()].
#'
#' [format.Portable()] returns a character string. It is a YAML representation
#' of `x`.
#'
#' [format.PortableTranslator()] returns the same output as [format.Portable()]
#' by default. If `set_instructions` is `TRUE`, it returns a character vector
#' of length 2:
#'
#'   1. a YAML string of comments, and
#'   2. the output of [format.Portable()].
#'
#' ## Outputs of `format.PortableTranslations()`
#'
#' [format.PortableTranslations()] returns a character vector. Its length
#' depends on `set_instructions`, and `how`.
#'
#'   1. If `how` is equal to `"yaml"`, it (minimally) returns the output of
#'      [format.Portable()], and this method is applied on `x` as a whole.
#'      If `set_instructions` is `FALSE`, it returns a character vector of
#'      length 2:
#'
#'      1. a string of YAML comments, and
#'      2. the output of [format.Portable()].
#'
#'   2. If `how` is equal to `"flat"`, it returns a character vector of
#'      length 2:
#'
#'      1. the output of [format.Portable()] applied on all elements of
#'         `x` except for field `translations`, and
#'      2. a character string of pairs of source text and translation
#'         constructed from field `translations`, and formatted according
#'         to what section Class PortableTranslations describes.
#'
#'      If `set_instructions` is `TRUE`, it returns a character vector of
#'      length 3. A YAML string of comments is appended to the elements listed
#'      above (as the first element).
#'
#' [print()] returns argument `x`, invisibly.
#'
#' @section Class PortableTranslator:
#' A [`PortableTranslator`][portable()] object is (minimally) a named list of
#' length 8 containing the following elements.
#'
#' \describe{
#'   \item{`version`}{A non-[NA][base::NA] integer value always equal to `1L`
#'     until further notice. This field is reserved for future usage. It will
#'     be used to ensure backwards compatibility and different templates.}
#'   \item{`generated_by`}{A non-[NA][base::NA] and non-empty character string.
#'     What generated the underlying object.}
#'   \item{`generated_on`}{See [utc()].}
#'   \item{`identifier`}{See active binding `id` of class
#'     [`Translator`][Translator].}
#'   \item{`hash_algorithm`}{See active binding `hash_algorithm` of class
#'     [`Translator`][Translator].}
#'   \item{`source_language`}{A non-empty and non-[NA][base::NA] character
#'     string. The common source language of all registered source texts.}
#'   \item{`languages`}{A named list. See active binding `native_languages` of
#'     class [`Translator`][Translator] for more information.}
#'   \item{`translations_files`}{A named list. File names for related
#'     [`PortableTranslations`][portable()] objects (one for each language
#'     listed in `languages`).}
#'   \item{`...`}{Further named [`PortableText`][portable()] objects, if any.}
#' }
#'
#' @section Class PortableText:
#' A [`PortableText`][portable()] object is a named list of length 6 containing
#' the following elements.
#'
#' \describe{
#'   \item{`hash`}{See active binding `hash` of class [`Text`][Text]. It is
#'     `NULL` if `source_language` is unset.}
#'   \item{`hash_algorithm`}{See active binding `hash_algorithm` of class
#'     [`Text`][Text].}
#'   \item{`source_language`}{See active binding `source_lang` of class
#'     [`Text`][Text]. It is `NULL` if unset.}
#'   \item{`source_text`}{See active binding `source_text` of class
#'     [`Text`][Text]. It is automatically wrapped to a maximum of 80
#'     characters per line. It is `NULL` if `source_language` is unset.}
#'   \item{`translations`}{A `NULL`, unless `set_translations` is `TRUE`. In
#'     that case, a named list, where elements are translations, and names are
#'     languages. Each translation is automatically wrapped to a maximum of 80
#'     characters per line.}
#'   \item{`locations`}{See active binding `locations` of class
#'     [`Text`][Text].}
#' }
#'
#' @section Class PortableLocation:
#' A [`PortableLocation`][portable()] is a named list of length 2 containing
#' the following elements.
#'
#' \describe{
#'   \item{`path`}{See argument `path` of [location()].}
#'   \item{`ranges`}{A character vector constructed from arguments `line1`,
#'     `col1`, `line2`, and `col2` of [location()]. See value `"long"` of
#'     argument `how` of [format.Location()] for the underlying format.}
#' }
#'
#' @section Class PortableTranslations:
#' This class has no corresponding *non-portable* class. It structures, and
#' groups source texts and translations by languages (which is preferable when
#' sharing translations).
#'
#' A [`PortableTranslations`][portable()] is a named list of length 5
#' containing the following elements.
#'
#' \describe{
#'   \item{`language`}{A non-empty and non-[NA][base::NA] character string.
#'     The language of the translations. See argument `lang` above for more
#'     information.}
#'   \item{`native_language`}{A non-empty and non-[NA][base::NA] character
#'     string. The underlying full native language name identified by
#'     `language`.}
#'   \item{`source_language`}{A non-empty and non-[NA][base::NA] character
#'     string. The language of the source texts. See `language` for more
#'     information.}
#'   \item{`source_native_language`}{A non-empty and non-[NA][base::NA]
#'     character string. The underlying full native language name identified
#'     by `source_language`.}
#'   \item{`translations`}{A named list. Its names are (reduced) hashes
#'     extracted from `x` (the underlying [`Translator`][Translator]). Its
#'     elements are named list of length 2 containing elements
#'     `source_text`, and `translation`. These are character strings, or `NULL`
#'     if unavailable, or unset.}
#' }
#'
#' ## Format
#'
#' A [`PortableTranslations`][portable()] has a dual format when `how` is set
#' equal to `flat`. In that case, all fields, except for `translations`, are
#' formatted as a YAML string via [format.Portable()]. Field `translations` is
#' formatted independently as a stream of plain text, unindented, and named
#' sections. The format is as follows.
#'
#' ```
#' [[hash]]
#'
#' A source text. Its hash (above) is determined automatically. The source
#' text and its hash must be left as is.
#'
#' [[Translation]]
#'
#' Its underlying translation, or a '<none>' placeholder to be removed by the
#' translator in charge of actually completing the section. Single line breaks
#' are interpreted as single spaces, like Markdown, and other text formats do.
#' This allows translators to avoid writing cumbersome, and unintuitive long
#' strings.
#'
#' [[another hash]]
#'
#' Another source text.
#'
#' [[Translation]]
#'
#' Another translation.
#'
#' ...
#' ```
#'
#' This stream is printed after the YAML string (which acts as a header).
#'
#' @note
#' Methods [as_location.Location()], and [as_text.Text()] are trivial
#' identity functions returning their argument. As such, they are never
#' useful. However, they are internally required by [translator_read()].
#'
#' A [`PortableTranslator`][portable()] object contains any number of
#' [`PortableText`][portable()] objects, and these objects further contain
#' any number of [`PortableLocation`][portable()] objects. Therefore,
#' [as_translator.PortableTranslator()] must call [as_text.PortableText()]
#' on each [`PortableText`][portable()] object, and [as_text.PortableText()]
#' must further call [as_location.PortableLocation()] on each
#' [`PortableLocation`][portable()] object. This follows the usual hierarchy
#' of [`transltr`][transltr] classes.
#'
#' When using these methods as *handlers* passed to [yaml::yaml.load()],
#' this hierarchy is reversed. [`PortableLocation`][portable()] objects are
#' converted **first** via [as_location.PortableLocation()], and passed as
#' [`Location`][Location] objects to the next handler,
#' [as_text.PortableText()]. The latter calls [as_location()] on each
#' [`Location`][Location] object, and therefore, a trivial
#' [as_location.Location()] method is required to accomodate this reversed
#' process. The same is true for [`Text`][Text] objects passed to
#' [as_translator.PortableTranslator()].
#'
#' Since [`PortableTranslator`][portable()] objects are the outer most objects,
#' [as_translator()] does not require an [as_translator.PortableTranslator()]
#' method.
#'
#' @seealso
#' [Official YAML 1.1 specification](https://yaml.org/spec/1.1/),
#' [translator_read()],
#' [translator_write()],
#' [translations_read()],
#' [translations_write()]
#'
#' @aliases
#' Portable
#' PortableTranslator
#' PortableText
#' PortableLocation
#' PortableTranslations
#'
#' @keywords internal
portable <- function(x, super = character(), tag = NULL) {
    assert_chr(super, TRUE)

    if (!is.null(tag)) {
        assert_chr1(tag)
        attr(x, "tag") <- tag
    }

    class(x) <- c(super, "Portable", class(x))
    return(x)
}

#' @rdname portable
#' @keywords internal
is_portable <- function(x) {
    return(inherits(x, "Portable"))
}

#' @rdname portable
#' @keywords internal
portable_translator <- function(x = translator(), set_translations = FALSE) {
    if (!is_translator(x)) {
        stops("'x' must be a 'Translator' object.")
    }
    if (length(source_lang <- x$source_langs) > 1L) {
        stops("all 'Text' objects of 'x' must have the same 'source_lang'.")
    }

    # It makes no sense to create a
    # TranslationsFile object for
    # the common source language.
    native_langs <- as.list(x$native_languages)
    langs <- names(native_langs)[-match(source_lang, names(native_langs), 0L)]

    # Construct list of PortableTranslations files.
    files <- as.list(sprintf("%s.txt", langs))
    names(files) <- langs
    attr(files, "tag") <- "TranslationsFiles"

    # Header fields are called as such because
    # they appear before PortableTexts in the
    # output.
    header <- list(
        version            = 1L,
        generated_by       = constant("generated-by"),
        generated_on       = utc(),
        identifier         = x$id,
        hash_algorithm     = x$hash_algorithm,
        source_language    = source_lang,
        languages          = native_langs,
        translations_files = files)

    # Extract Text objects and create
    # PortableText objects from them.
    texts <- lapply(
        lapply(x$hashes, x$get_text),
        portable_text,
        set_translations = set_translations)

    out <- c(header, texts)
    return(portable(out, "PortableTranslator", "Translator"))
}

#' @rdname portable
#' @keywords internal
portable_text <- function(x = text(), set_translations = FALSE) {
    if (!is_text(x)) {
        stops("'x' must be a 'Text' object.")
    }

    assert_lgl1(set_translations)

    # hash_algorithm and locations
    # cannot be unset by design.
    out <- list(
        hash            = NULL,
        hash_algorithm  = x$hash_algorithm,
        source_language = NULL,
        source_text     = NULL,
        translations    = NULL,
        locations       = lapply(x$locations, portable_location))

    if (x$hash != constant("unset")) {
        out$hash            <- x$hash
        out$source_language <- x$source_lang
        out$source_text     <- strwrap(x$source_text, 80L)
    }
    if (set_translations) {
        # Source text is already extracted above.
        texts <- x$translations[names(x$translations) != x$source_lang]

        # Lists are preferable when using yaml::as.yaml()
        # because names are retained and used as keys.
        out$translations <- lapply(texts, strwrap, 80L)
    }

    names(out$locations) <- NULL
    return(portable(out, "PortableText", "Text"))
}

#' @rdname portable
#' @keywords internal
portable_location <- function(x = location()) {
    if (!is_location(x)) {
        stops("'x' must be a 'Location' object.")
    }

    out <- list(
        path   = x$path,
        ranges = .location_format_range(x))

    return(portable(out, "PortableLocation", "Location"))
}

#' @rdname portable
#' @keywords internal
portable_translations <- function(
    x           = translator(),
    lang        = NULL,
    placeholder = constant("placeholder"))
{
    if (!is_translator(x)) {
        stops("'x' must be a 'Translator' object.")
    }

    assert_chr1(placeholder)

    texts        <- lapply(x$hashes, x$get_text)
    native_langs <- x$native_languages
    source_lang  <- x$source_langs

    # Since all source_lang must be equal,
    # it does not matter which one is chosen.
    if (length(source_lang) > 1L) {
        stops("all 'Text' objects of 'x' must have the same 'source_lang'.")
    }

    # It makes no sense to create a
    # PortableTranslations object
    # for the common source language.
    langs <- names(native_langs)[-match(source_lang, names(native_langs), 0L)]

    if (!is.null(lang)) {
        assert_match(lang, langs, quote_values = TRUE)
    }

    lang <- lang %??% langs
    out  <- lapply(lang, \(lang) {
        out <- list(
            language               = lang,
            native_language        = native_langs[[lang]],
            source_language        = source_lang,
            source_native_language = native_langs[[source_lang]],
            translations           = lapply(texts, \(txt) {
                source <- txt$source_text
                text   <- txt$get_translation(lang)

                # strwrap() returns character(0)
                # if input is NULL, but we need
                # to preserve NULLs.
                return(
                    list(
                        source_text = if (is.null(source)) placeholder else strwrap(source, 80L),
                        translation = if (is.null(text))   placeholder else strwrap(text,   80L)))
        }))

        return(portable(out, "PortableTranslations", "Translations"))
    })

    names(out) <- lang
    return(out)
}

#' @rdname portable
#' @export
format.Portable <- function(x, ...) {
    return(
        yaml::as.yaml(x,
            line.sep = "\n",
            indent   = 2L,
            unicode  = TRUE,
            indent.mapping.sequence = TRUE))
}

#' @rdname portable
#' @export
format.PortableTranslator <- function(x, set_instructions = FALSE, ...) {
    assert_lgl1(set_instructions)

    instructions <- if (set_instructions) {
        paste0(
            "# Portable Translator\n",
            "#\n",
            "# Instructions:\n",
            "#  - You may edit fields identifier, languages, and translations_files.\n",
            "#  - Do not edit other fields by hand; edit source scripts instead.\n",
            "#  - Translations are stored in separate files. See translations_files.\n",
            "%YAML 1.1\n",
            "---")
    }

    return(c(instructions, NextMethod(x)))
}

#' @rdname portable
#' @export
format.PortableTranslations <- function(x,
    how              = c("yaml", "flat"),
    set_instructions = FALSE,
    placeholder      = constant("placeholder"),
    ...)
{
    # NOTE: this function implements the flat format.
    # It is tightly coupled with translation_read().

    assert_lgl1(set_instructions)
    assert_arg(how, TRUE)

    instructions <- if (set_instructions) {
        paste0(
            "# Portable Translations\n",
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
    }

    switch(how,
        yaml = return(c(instructions, NextMethod(x))),
        flat = {
            trans <- x$translations

            # Create flat sections. A flat section begins
            # with a [[ title ]] line, and whatever comes
            # after is its contents.
            main <- paste0(collapse = "\n\n", sprintf(
                "[[%s]]\n\n%s\n\n[[Translation]]\n\n%s",
                names(trans),
                vapply_1c(trans, `[[`, "source_text"),
                vapply_1c(trans, `[[`, "translation")))

            # Translations are removed because
            # they are formatted as flat sections.
            x$translations <- NULL
            return(c(instructions, NextMethod(x), main))
        }
    )
}

#' @rdname portable
#' @export
print.Portable <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    return(invisible(x))
}

#' @rdname portable
#' @export
as_translator.PortableTranslator <- function(
    x,
    set_translations_files = TRUE,
    ...)
{
    assert_lgl1(set_translations_files)

    id             <- x$identifier
    hash_algorithm <- x$hash_algorithm
    trans          <- Translator$new(id, hash_algorithm)

    if (set_translations_files) {
        attr(trans, "translations_files") <- x$translations_files
    }

    # FIXME: why is this needed, and as_text.PortableText() does not need it?
    # Not so sure why Texts are not registered.
    is_txt <- vapply_1l(x, \(x) {
        tag <- attr(x, which = "tag")
        return(is_text(x) || (!is.null(tag) && tag == "Text"))
    })

    do.call(trans$set_texts, lapply(x[is_txt], as_text))
    do.call(trans$set_native_languages, x$languages)
    return(trans)
}

#' @rdname portable
#' @export
as_text.PortableText <- function(x, ...) {
    # `[[` must be used here to prevent partial matching
    # to hash_algorithm in the unlikely case that hash is
    # missing. This could happen if users mess with their
    # portable objects.
    label <- sprintf("Text '%s': ", x[["hash"]] %??% constant("unknown"))
    txt   <- Text$new(hash_algorithm <- x$hash_algorithm)

    do.call(txt$set_locations, lapply(x$locations, as_location))
    do.call(txt$set_translations, x$translations %??% list())

    if (!is.null(source_language <- x$source_language)) {
        # This can only happen if a PortableTranslator
        # (the underlying YAML file) is edited by hand.
        if (is.null(x$source_text)) {
            warning(call. = FALSE,
                label,
                "'source_language' is defined, but not 'source_text'.")
        }

        source_text <- text_normalize(x$source_text)
        txt$set_translation(source_language, source_text)
        txt$source_lang <- source_language
    }
    if (x[["hash"]] %??% constant("unset") != txt$hash) {
        warning(call. = FALSE,
            label,
            sprintf("hash is not equal to expected hash ('%s').", txt$hash),
            " The latter will be used.")
    }

    return(txt)
}

#' @rdname portable
#' @export
as_text.Text <- function(x, ...) {
    return(x)
}

#' @rdname portable
#' @export
as_location.PortableLocation <- function(x, ...) {
    # Extract digits from ranges, construct
    # character vectors of numbers, and try
    # converting them to integers.
    ranges <- x$ranges
    chrs   <- regmatches(ranges, gregexpr("[0123456789]+", ranges))
    ints   <- suppressWarnings(lapply(chrs, as.integer))

    # Validate resulting integer vectors.
    # There should be one for each range.
    # The callback returns nothing.
    map(ints, seq_along(ints), fun = \(ints, index) {
        if (length(ints) != 4L || anyNA(ints) || !all(ints > 0L)) {
            stopf("could not convert range '%s'.", ranges[[index]])
        }
    })

    return(
        location(x$path,
            vapply_1i(ints, `[[`, 1L),
            vapply_1i(ints, `[[`, 2L),
            vapply_1i(ints, `[[`, 3L),
            vapply_1i(ints, `[[`, 4L)))
}

#' @rdname portable
#' @export
as_location.Location <- function(x, ...) {
    return(x)
}
