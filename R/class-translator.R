#' Source Texts, and Translations
#'
#' Store, structure, and manipulate source text**s**, and their translations.
#'
#' A [`Translator`][Translator] object encapsulates the source text of a
#' project (or any other *context*), and all related translations. It exposes
#' a set of methods that can be used to manipulate this information, but it
#' is designed in such a way that its methods can be ignored most of the time.
#'
#' Under the hood, [`Translator`][Translator] objects are collections of
#' [`Text`][Text] objects. These do most of the work. They are treated as
#' lower-level component, and in typical situations, users rarely interact
#' with them.
#'
#' ## Translating Text
#'
#' Since it can be detected and processed by [find_source()], it is recommended
#' to use [translate()] at all times. Method
#' [`Translator$translate()`][Translator] is the underlying workhorse function
#' called by the former.
#'
#' ## Exporting, and Importing Translators
#'
#' [`Translator`][Translator] objects can be saved, and exported with
#' [translator_write()].
#'
#' They can be imported back into an \R session with [translator_read()].
#'
#' @param ... Usage depends on the underlying function.
#'   * Any number of [`Text`][Text] objects, and/or named character
#'     strings for [translator()] (in no preferred order).
#'   * Further arguments passed to or from other methods for [format()],
#'     and [print()].
#'
#' @param x Any \R object.
#'
#' @template param-id
#'
#' @template param-hash-algorithm
#'
#' @returns
#' [translator()], and [as_translator()] return an [`R6`][R6::R6] object of
#' class [`Translator`][Translator].
#'
#' [is_translator()] returns a logical value.
#'
#' [format()] returns a character vector.
#'
#' [print()] returns argument `x` invisibly.
#'
#' @seealso [translate()],
#'   [translator_set()],
#'   [translator_get()],
#'   [translator_import()],
#'   [translator_export()]
#'
#' @examples
#' #' ## Set source language.
#' language_source_set("en")
#'
#' ## Create a Translator manually.
#' translator(
#'   id = "test-translator",
#'   en = "English",
#'   fr = "Français",
#'   text(
#'     location("a", 1L, 2L, 3L, 4L),
#'     en = "Hello, world!",
#'     fr = "Bonjour, monde!"),
#'   text(
#'     location("b", 5L, 6L, 7L, 8L),
#'     en = "Farewell, world!",
#'     fr = "Au revoir, monde!"))
#'
#' @include constants.R
#' @rdname class-translator
#' @export
translator <- function(..., id = uuid(), hash_algorithm = hash_algorithms()) {
    trans <- Translator$new(id, hash_algorithm)
    dots  <- list(...)

    do.call(trans$set_texts, dots[vapply_1l(dots, is_text)])
    do.call(trans$set_native_languages, dots[vapply_1l(dots, is.character)])

    # All language codes should have a
    # corresponding native language name.
    if (anyNA(m <- match(trans$languages, names(trans$native_languages)))) {
        warning(call. = FALSE, sprintf(
            "some languages are missing an equivalent native language name: %s.",
            to_string(trans$languages[is.na(m)], TRUE, ", and ")))
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
    if (!is.null(x$source_texts)) {
        texts <- lapply(x$hashes, x$get_text)
        langs  <- lapply(texts, \(b) to_string(b$languages, last_sep = ", "))

        source_texts <- x$source_texts
        hashes <- names(source_texts)  ## These are already reduced.
        names(source_texts) <- sprintf("%s [%s]", hashes, langs)
    } else {
        source_texts <- constant("placeholder")
    }

    xlist <- list(
        Identifier     = x$id,
        Algorithm      = x$hash_algorithm,
        Languages      = x$native_languages %??% constant("empty"),
        `Source Texts` = source_texts)

    return(format_vector(xlist, "<Translator>", .show_nokey = FALSE))
}

#' @rdname class-translator
#' @export
print.Translator <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    return(invisible(x))
}

#' @rdname class-translator
#' @export
as_translator <- function(x, ...) {
    UseMethod("as_translator")
}

#' @rdname class-translator
#' @export
Translator <- R6::R6Class("Translator",
    lock_class   = TRUE,
    lock_objects = TRUE,
    cloneable    = FALSE,
    private      = list(
        .id           = constant("unset"), # See $id
        .hash_algo    = constant("unset"), # See $hash_algorithm
        .native_langs = NULL,              # See $native_languages, $initialize -> new.env()
        .texts       = NULL,               # See $intialize -> new.env()

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

        #' @template field-hash-algorithm
        hash_algorithm = \(value) {
            if (!missing(value)) {
                assert_chr1(value, x_name = "hash_algorithm")
                assert_match(value, hash_algorithms(),
                    quote_values = TRUE,
                    x_name       = "hash_algorithm")

                # Update hash_algorithm of each Text.
                eapply(private$.texts, \(txt) txt$hash_algorithm <- value)

                # Update object's hash_algorithm.
                private$.hash_algo <- value

                # Reassign updated Text under new hashes.
                texts          <- as.list(private$.texts)
                private$.texts <- new.env(parent = emptyenv())
                do.call(self$set_texts, texts)
            }

            return(private$.hash_algo)
        },

        #' @field hashes A character vector of non-empty and non-[NA][base::NA]
        #'   values, or `NULL`. The set of all `hash` exposed by registered
        #'   [`Text`][Text] objects. If there is none, `hashes` is `NULL`.
        #'   This is a **read-only** field. It is automatically updated
        #'   whenever field `hash_algorithm` is updated.
        hashes = \(value) {
            if (!missing(value)) {
                stops(
                    "'hashes' cannot be overwritten.\n",
                    "Update them by setting 'hash_algorithm', and by setting, or removing 'Text' objects.")
            }

            hashes <- eapply(private$.texts, `[[`, i = "hash")
            names(hashes) <- private$.hash_reduce(names(hashes))
            return(unlist(hashes))
        },

        #' @field source_texts A character vector of non-empty and
        #'   non-[NA][base::NA] values, or `NULL`. The set of all
        #'   `source_text` exposed by registered [`Text`][Text]
        #'   objects. If there is none, `source_texts` is `NULL`.
        #'   This is a **read-only** field.
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

        #' @field source_langs A character vector of non-empty and
        #'   non-[NA][base::NA] values, or `NULL`. The set of all
        #'   `source_text` exposed by registered [`Text`][Text]
        #'   objects. This is a **read-only** field.
        #'   * If there is none, `source_texts` is `NULL`.
        #'   * If there is one unique value, `source_texts` has
        #'     a length equal to 1.
        #'   * Otherwise, a named character vector is returned.
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

        #' @field languages A character vector of non-empty and
        #'   non-[NA][base::NA] values, or `NULL`. The set of all
        #'   `languages` (codes) exposed by registered [`Text`][Text]
        #'   objects. If there is none, `languages` is `NULL`.
        #'   This is a **read-only** field.
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
        #'   non-[NA][base::NA] values, or `NULL`. An optional, but useful map
        #'   of `languages` (codes) to native language names. Names are codes,
        #'   and values are native languages. If there is none,
        #'   `native_languages` is `NULL`.
        #'
        #'   While users retain full control over `native_languages`, it is
        #'   best to use well-known schemes such as
        #'   [IETF BCP 47](https://en.wikipedia.org/wiki/IETF_language_tag), or
        #'   [ISO 639-1](https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes).
        #'   Doing so maximizes portability and cross-compatibility between packages.
        #'
        #'   Update this field with method
        #'   [`Translator$set_native_languages()`][Translator]. See below for
        #'   more information.
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
        #' @template param-hash-algorithm
        #'
        #' @return An [`R6`][R6::R6] object of class [`Translator`][Translator].
        initialize = \(id = uuid(), hash_algorithm = hash_algorithms()) {
            assert_chr1(id)
            assert_arg(hash_algorithm, TRUE)

            private$.id           <- id
            private$.hash_algo    <- hash_algorithm
            private$.native_langs <- new.env(parent = emptyenv())
            private$.texts        <- new.env(parent = emptyenv())
            return(self)
        },

        #' @description Translate text. Consider using [translate()] instead
        #'   of this method.
        #'
        #' @details
        #' Values passed to `...` are first normalized by [text_normalize()],
        #' and the resulting string is hashed by [text_hash()]. Then, the
        #' corresponding translation tied to the `hash` and `lang` pair is
        #' fetched via method [`Translator$get_translation()`][Translator].
        #'
        #  NOTE: Package roxygen2 reuses templates whenever within an R6 class.
        #
        #' @template param-dots-source-text
        #'
        #' @template param-lang
        #'
        #' @template param-concat
        #'
        #' @template param-source-lang-no-example
        #'
        #' @return A character string, or `NULL`. The corresponding translation
        #'   in the requested language, or `NULL` if none is available.
        translate = \(
            ...,
            lang        = language_get(),
            concat      = constant("concat"),
            source_lang = language_source_get())
        {
            assert_chr1(lang)
            assert_chr1(concat, TRUE)
            assert_chr1(source_lang)

            text <- text_normalize(..., .concat = concat)
            hash <- text_hash(source_lang, text, private$.hash_algo)
            return(self$get_translation(hash, lang))
        },

        #' @description Extract a translation, or source texts.
        #'
        #' @template param-hash
        #'
        #' @return A character string. `NULL` is returned if the requested
        #'   translation is not available (either `hash` or `lang` is not
        #'   registered).
        get_translation = \(hash = "", lang = "") {
            if (is.null(txt <- self$get_text(hash))) {
                return(NULL)
            }

            return(txt$get_translation(lang))
        },

        #' @description Extract a [`Text`][Text] object.
        #'
        #' @return A [`Text`][Text] object, or `NULL`.
        get_text = \(hash = "") {
            assert_chr1(hash)
            return(private$.texts[[private$.hash_expand(hash)]])
        },

        #' @description Simultaneously create and register a [`Text`][Text]
        #'   object.
        #'
        #' @param ... Passed as is to [text()].
        #'
        #' @param source_lang Passed as is to [text()].
        #'
        #' @return A `NULL`, invisibly.
        set_text = \(..., source_lang = language_source_get()) {
            blk <- text(...,
                source_lang    = source_lang,
                hash_algorithm = private$.hash_algo)

            self$set_texts(blk)
            return(invisible())
        },

        #' @description Register one or more [`Text`][Text] objects.
        #'
        #' @param ... Any number of [`Text`][Text] objects.
        #'
        #' @details This method calls [merge_texts()] to merge all
        #'   values passed to `...` together with previously registered
        #'   [`Text`][Text] objects. The underlying registered source
        #'   texts, translations, and [`Location`][Location] objects
        #'   won't be duplicated.
        #'
        #' @return A `NULL`, invisibly.
        #'
        #' @examples
        #' ## Set source language.
        #' language_source_set("en")
        #'
        #' ## Create Text objects.
        #' blk1 <- text(
        #'   location("a", 1L, 2L, 3L, 4L),
        #'   en = "Hello, world!",
        #'   fr = "Bonjour, monde!")
        #' blk2 <- text(
        #'   location("b", 5L, 6L, 7L, 8L),
        #'   en = "Farewell, world!",
        #'   fr = "Au revoir, monde!")
        #'
        #' ## Create a new Translator and register them.
        #' trans <- Translator$new()
        #' trans$set_texts(blk1, blk2)
        set_texts = \(...) {
            if (!...length()) {
                return(invisible())
            }

            args   <- c(as.list(private$.texts), list(...), hash_algorithm = private$.hash_algo)
            texts <- do.call(merge_texts, args)
            names(texts) <- vapply_1c(texts, `[[`, i = "hash")

            list2env(texts, private$.texts)
            return(invisible())
        },

        #' @description Map a language code to a native language name.
        #'
        #' @param ... Any number of named, non-empty, and non-[NA][base::NA]
        #'   character strings. Names are codes, and values are native
        #'   languages. See field `native_languages` for more information.
        #'
        #' @return A `NULL`, invisibly.
        #'
        #' @examples
        #' trans <- Translator$new()
        #' trans$set_native_languages(
        #'   en = "English",
        #'   fr = "Français",
        #'   ja = "日本語")
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

        #' @description Remove a registered location.
        #'
        #' @param hash A non-empty and non-[NA][base::NA] character string
        #'   identifying the [`Text`][Text] object to be removed.
        #'
        #' @return A `NULL`, invisibly.
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
        }
    )
)
