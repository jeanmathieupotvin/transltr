#' Source Text and Translations
#'
#' Structure and manipulate the source text of a project and its translations.
#'
#' A [`Translator`][Translator] object encapsulates the source text of a project
#' (or any other *context*) and all related translations. Under the hood,
#' [`Translator`][Translator] objects are collections of [`Text`][Text] objects.
#' These do most of the work. They are treated as lower-level component and in
#' typical situations, users rarely interact with them.
#'
#' [`Translator`][Translator] objects can be saved and exported with
#' [translator_write()]. They can be imported back into an \R session
#' with [translator_read()].
#'
#' @param ... Usage depends on the underlying function.
#'
#'   * Any number of [`Text`][Text] objects and/or named character
#'     strings for [translator()] (in no preferred order).
#'   * Further arguments passed to or from other methods for [format()],
#'     and [print()].
#'
#' @param x Any \R object.
#'
#' @template param-id
#'
#' @template param-algorithm
#'
#' @returns
#' [translator()] returns an [`R6`][R6::R6] object of class
#' [`Translator`][Translator].
#'
#' [is_translator()] returns a logical value.
#'
#' [format()] returns a character vector.
#'
#' [print()] returns argument `x` invisibly.
#'
#' @seealso
#' [find_source()],
#' [translator_read()],
#' [translator_write()]
#'
#' @examples
#' # Set source language.
#' language_source_set("en")
#'
#' # Create a Translator object.
#' # This would normally be done automatically
#' # by find_source(), or translator_read().
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
#' is_translator(tr)
#'
#' # Translator objects has a specific format.
#' # print() calls format() internally, as expected.
#' print(tr)
#'
#' @rdname class-translator
#' @export
translator <- function(..., id = uuid(), algorithm = algorithms()) {
    trans <- Translator$new(id, algorithm)
    dots  <- list(...)

    do.call(trans$set_texts, dots[vapply_1l(dots, is_text)])
    do.call(trans$set_native_languages, dots[vapply_1l(dots, is.character)])

    # All language codes should have a
    # corresponding native language name.
    if (anyNA(m <- match(trans$languages, names(trans$native_languages)))) {
        warning(call. = FALSE, sprintf(
            "some languages are missing an equivalent native language name: %s.",
            str_to(trans$languages[is.na(m)], TRUE, ", and ")))
    }

    return(trans)
}

#' @rdname class-translator
#' @export
is_translator <- function(x) {
    return(inherits(x, "Translator"))
}

#' @rdname class-translator
#' @export
format.Translator <- function(x, ...) {
    # Each source text is printed on a single line:
    # <reduced-hash> [<lang>, ...]: <source-text>.
    # Long lines are truncated by format_vector().
    if (!is.null(source_texts <- x$source_texts)) {
        # Escape newlines to preserve format.
        source_texts <- structure(
            stringi::stri_replace_all_regex(x$source_texts, "\n", r"{\\n}"),
            names = names(source_texts))

        langs <- lapply(x$hashes, \(h) {
            str_to(x$get_text(h)$languages, last_sep = ", ")
        })

        # Names are used as labels by format_vector().
        names(source_texts) <- sprintf("%s [%s]", names(source_texts), langs)
    }

    xlist <- list(
        Identifier    = x$id,
        Algorithm     = x$algorithm,
        Languages     = x$native_languages,
        `Source Text` = source_texts)

    return(c("<Translator>", format_vector(xlist, level = 1L)))
}

#' @rdname class-translator
#' @export
print.Translator <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    return(invisible(x))
}

#' @rdname class-translator
#' @export
Translator <- R6::R6Class("Translator",
    lock_class   = TRUE,
    lock_objects = TRUE,
    cloneable    = FALSE,
    private      = list(
        .id            = .__STR_UNSET,  # See $id
        .algorithm     = .__STR_UNSET,  # See $algorithm
        .native_langs  = NULL,          # See $native_languages, $initialize -> new.env()
        .texts         = NULL,          # See $intialize -> new.env()
        .default_value = NULL,

        # @description Compress a hash.
        #
        # @param hashes A character vector. Its elements can be empty or NA.
        #
        # @return A character vector having the same length as `hashes`. The
        #   minimal enforced length is set equal to 7 (whenever possible).
        .hash_reduce = \(hashes = character()) {
            return(
                abbreviate(
                    hashes,
                    minlength   = 7L,
                    use.classes = FALSE,
                    dot         = FALSE,
                    strict      = TRUE,
                    named       = TRUE))
        },

        # @description Decompress a hash and get back its full value.
        #
        # @param hash A character string. It can be empty or NA.
        #
        # @return A character string. It returns "<notfound>" if `hash` does
        #   not match a registered (full) hash.
        .hash_expand  = \(hash = "") {
            hashes <- names(private$.texts)
            pos    <- charmatch(hash, hashes, 0L)

            # Any string that is not a registered hash
            # could be returned in case hash cannot be
            # expanded, because `[[` returns NULL no
            # matter what it is (it is mostly irrelevant).
            return(if (pos) hashes[[pos]] else "<notfound>")
        }
    ),
    active = list(
        #' @template field-id
        id = \(value) {
            if (!missing(value)) {
                assert_chr1(value, x_name = "id")
                private$.id <- value
            }

            return(private$.id)
        },

        #' @template field-algorithm
        algorithm = \(value) {
            if (!missing(value)) {
                assert_algorithm <- \(algorithm = algorithms()) {
                    assert_arg(algorithm, TRUE)
                    return(algorithm)
                }

                value <- assert_algorithm(value)

                # Update algorithm of each Text.
                eapply(private$.texts, \(txt) txt$algorithm <- value)

                # Update object's algorithm.
                private$.algorithm <- value

                # Reassign updated Text under new hashes.
                texts          <- as.list(private$.texts)
                private$.texts <- new.env(parent = emptyenv())
                do.call(self$set_texts, texts)
            }

            return(private$.algorithm)
        },

        #' @field hashes A character vector of non-empty and non-NA values, or
        #'   `NULL`. The set of all `hash` exposed by registered [`Text`][Text]
        #'   objects. If there is none, `hashes` is `NULL`. This is a
        #'   **read-only** field updated whenever field `algorithm` is updated.
        hashes = \(value) {
            if (!missing(value)) {
                stops(
                    "'hashes' cannot be overwritten.\n",
                    "Update them by setting 'algorithm' and by setting, or removing 'Text' objects.")
            }

            hashes <- eapply(private$.texts, `[[`, i = "hash")
            names(hashes) <- private$.hash_reduce(names(hashes))
            return(unlist(hashes))
        },

        #' @field source_texts A character vector of non-empty and non-NA
        #'   values, or `NULL`. The set of all registered source texts. If
        #'   there is none, `source_texts` is `NULL`. This is a **read-only**
        #'   field.
        source_texts = \(value) {
            if (!missing(value)) {
                stops(
                    "'source_texts' cannot be overwritten.\n",
                    "Update them by setting, or removing 'Text' objects.")
            }
            if (!length(private$.texts)) {
                # Assigning names to NULL would
                # throw a non-semantic error.
                return(NULL)
            }

            texts <- unlist(eapply(private$.texts, `[[`, i = "source_text"))
            names(texts) <- private$.hash_reduce(names(texts))
            return(texts)
        },

        #' @field source_langs A character vector of non-empty and non-NA
        #'   values, or `NULL`. The set of all registered source languages.
        #'   This is a **read-only** field.
        #'
        #'   * If there is none, `source_langs` is `NULL`.
        #'   * If there is one unique value, `source_langs` is an unnamed
        #'     character string.
        #'   * Otherwise, it is a named character vector.
        source_langs = \(value) {
            if (!missing(value)) {
                stops(
                    "'source_langs' cannot be overwritten.\n",
                    "Update them by setting, or removing 'Text' objects.")
            }
            if (!length(private$.texts)) {
                # Otherwhise, assigning names to NULL would
                # throw a non-semantic error. See below.
                return(NULL)
            }

            langs <- unlist(eapply(private$.texts, `[[`, i = "source_lang"))

            if (length(unique(langs)) > 1L) {
                names(langs) <- private$.hash_reduce(names(langs))
                return(langs)
            }

            return(langs[[1L]])
        },

        #' @field languages A character vector of non-empty and non-NA values,
        #'   or `NULL`. The set of all registered `languages` (codes). If there
        #'   is none, `languages` is `NULL`. This is a **read-only** field.
        languages = \(value) {
            if (!missing(value)) {
                stops(
                    "'languages' cannot be overwritten.\n",
                    "Update them by setting, or removing 'Text' objects.")
            }

            langs <- eapply(private$.texts, `[[`, i = "languages")
            return(sort(unique(unlist(langs, use.names = FALSE))))
        },

        #' @field native_languages A named character vector of non-empty and
        #'   non-NA values, or `NULL`. A map (bijection) of `languages` (codes)
        #'   to native language names. Names are codes and values are native
        #'   languages. If there is none, `native_languages` is `NULL`.
        #'
        #'   While users retain full control over `native_languages`, it is
        #'   best to use well-known schemes such as
        #'   [IETF BCP 47](https://en.wikipedia.org/wiki/IETF_language_tag), or
        #'   [ISO 639-1](https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes).
        #'   Doing so maximizes portability and cross-compatibility between packages.
        #'
        #'   Update this field with method [`$set_native_languages()`][Translator].
        #'   See below for more information.
        native_languages = \(value) {
            if (!missing(value)) {
                stops("use 'set_native_languages()' to update 'native_languages'.")
            }

            # It is important to not return the OG
            # environment in order to preserve the
            # object's internal state.
            return(unlist(as.list(private$.native_langs, sorted = TRUE)))
        }
    ),
    public = list(
        #' @description Create a [`Translator`][Translator] object.
        #'
        #' @template param-id
        #'
        #' @template param-algorithm
        #'
        #' @return An [`R6`][R6::R6] object of class [`Translator`][Translator].
        #'
        #' @examples
        #' # Consider using translator() instead.
        #' tr <- Translator$new()
        initialize = \(id = uuid(), algorithm = algorithms()) {
            private$.native_langs <- new.env(parent = emptyenv())
            private$.texts        <- new.env(parent = emptyenv())

            self$id        <- id
            self$algorithm <- algorithm
            return(self)
        },

        #' @description Translate source text.
        #'
        #  NOTE: Package roxygen2 reuses templates whenever within an R6 class.
        #
        #' @param source_lang A non-empty and non-NA character string. The
        #'   language of the source text. See argument `lang` for more
        #'   information.
        #'
        #' @template param-dots-source-text
        #'
        #' @template param-lang
        #'
        #' @details
        #' See [normalize()] for further details on how `...` is normalized.
        #'
        #' @return A character string. If there is no corresponding translation,
        #'   the value passed to method [`$set_default_value()`][Translator] is
        #'   returned. `NULL` is returned by default.
        #'
        #' @examples
        #' tr <- Translator$new()
        #' tr$set_text(en = "Hello, world!", fr = "Bonjour, monde!")
        #' tr$translate("Hello, world!", lang = "en")  ## Outputs "Hello, world!"
        #' tr$translate("Hello, world!", lang = "fr")  ## Outputs "Bonjour, monde!"
        translate = \(
            ...,
            lang        = language_get(),
            source_lang = language_source_get())
        {
            assert_chr1(source_lang)
            text <- normalize(...)
            hash <- hash(source_lang, text, self$algorithm)
            return(self$get_translation(hash, lang))
        },

        #' @description Extract a translation or a source text.
        #'
        #' @template param-hash
        #'
        #' @return A character string. If there is no corresponding translation,
        #'   the value passed to method [`$set_default_value()`][Translator] is
        #'   returned. `NULL` is returned by default.
        #'
        #' @examples
        #' tr <- Translator$new()
        #' tr$set_text(en = "Hello, world!")
        #'
        #' # Consider using translate() instead.
        #' tr$get_translation("256e0d7", "en")  ## Outputs "Hello, world!"
        get_translation = \(hash = "", lang = "") {
            if (is.null(txt <- self$get_text(hash))) {
                return(private$.default_value)
            }

            return(txt$get_translation(lang) %??% private$.default_value)
        },

        #' @description Extract a source text and its translations.
        #'
        #' @return A [`Text`][Text] object, or `NULL`.
        #'
        #' @examples
        #' tr <- Translator$new()
        #' tr$set_text(en = "Hello, world!")
        #'
        #' tr$get_translation("256e0d7", "en")  ## Outputs "Hello, world!"
        get_text = \(hash = "") {
            assert_chr1(hash)
            return(private$.texts[[private$.hash_expand(hash)]])
        },

        #' @description Register a source text.
        #'
        #' @param ... Passed as is to [text()].
        #'
        #' @param source_lang Passed as is to [text()].
        #'
        #' @return A `NULL`, invisibly.
        #'
        #' @examples
        #' tr <- Translator$new()
        #'
        #' tr$set_text(en = "Hello, world!", location())
        set_text = \(..., source_lang = language_source_get()) {
            txt <- text(...,
                source_lang = source_lang,
                algorithm   = self$algorithm)

            self$set_texts(txt)
            return(invisible())
        },

        #' @description Register one or more source texts.
        #'
        #' @param ... Any number of [`Text`][Text] objects.
        #'
        #' @details This method calls [merge_texts()] to merge all values
        #'   passed to `...` together with previously registered
        #'   [`Text`][Text] objects. The underlying registered source texts,
        #'   translations, and [`Location`][Location] objects won't be
        #'   duplicated.
        #'
        #' @return A `NULL`, invisibly.
        #'
        #' @examples
        #' # Set source language.
        #' language_source_set("en")
        #'
        #' tr <- Translator$new()
        #'
        #' # Create Text objects.
        #' txt1 <- text(
        #'   location("a", 1L, 2L, 3L, 4L),
        #'   en = "Hello, world!",
        #'   fr = "Bonjour, monde!")
        #'
        #' txt2 <- text(
        #'   location("b", 5L, 6L, 7L, 8L),
        #'   en = "Farewell, world!",
        #'   fr = "Au revoir, monde!")
        #'
        #' tr$set_texts(txt1, txt2)
        set_texts = \(...) {
            if (!...length()) {
                return(invisible())
            }

            args <- c(
                as.list(private$.texts),
                list(...),
                algorithm = self$algorithm)

            texts <- do.call(merge_texts, args)
            names(texts) <- vapply_1c(texts, `[[`, i = "hash")

            list2env(texts, private$.texts)
            return(invisible())
        },

        #' @description Remove a registered source text.
        #'
        #' @param hash A non-empty and non-NA character string identifying the
        #'   source text to remove.
        #'
        #' @return A `NULL`, invisibly.
        #'
        #' @examples
        #' tr <- Translator$new()
        #' tr$set_text(en = "Hello, world!")
        #'
        #' tr$rm_text("256e0d7")
        rm_text = \(hash = "") {
            if (!length(private$.texts)) {
                stops("there are no registered 'Text' objects to remove.")
            }

            assert_chr1(hash)
            assert_match(hash,
                private$.hash_reduce(self$hashes),
                quote_values = TRUE)

            rm(list = private$.hash_expand(hash), envir = private$.texts)
            return(invisible())
        },

        #' @description Map a language code to a native language name.
        #'
        #' @param ... Any number of named, non-empty, and non-NA character
        #'   strings. Names are codes and values are native languages. See
        #'   field `native_languages` for more information.
        #'
        #' @return A `NULL`, invisibly.
        #'
        #' @examples
        #' tr <- Translator$new()
        #'
        #' tr$set_native_languages(en = "English", fr = "Français")
        #'
        #' # Remove existing entries.
        #' tr$set_native_languages(fr = NULL)
        set_native_languages = function(...) {
            if (!...length()) {
                return(invisible())
            }
            if (!all(vapply_1l(dots <- list(...), \(x) is.null(x) || is_chr1(x)))) {
               stops(
                "values passed to '...' must all be character strings or 'NULL'.\n",
                "Use 'NULL' to remove entries.")
            }

            assert_named(dots, x_name = "...")
            list2env(dots, private$.native_langs)
            return(invisible())
        },

        #' @description Register a default value to return when there is no
        #'   corresponding translations for the requested language.
        #'
        #' @param value A `NULL` or a non-NA character string. It can be empty.
        #'   The former is returned by default.
        #'
        #' @details This modifies what methods [`$translate()`][Translator] and
        #'   [`$get_translation()`][Translator] returns when there is no
        #'   translation for `lang`.
        #'
        #' @return A `NULL`, invisibly.
        #'
        #' @examples
        #' tr <- Translator$new()
        #' tr$set_default_value("<unavailable>")
        set_default_value = function(value = NULL) {
            if (!is.null(value)) {
                assert_chr1(value, TRUE)
                private$.default_value <- value
            }

            return(invisible())
        }
    )
)
