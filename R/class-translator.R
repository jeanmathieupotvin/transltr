#' Source Texts and Translations
#'
#' Store, structure, and manipulate source text**s** and their translations.
#'
#' A [`Translator`][Translator] object is a collection of many source texts,
#' and related translations. It exposes a set of methods that can be used to
#' safely manipulate the information it contains. Since [translator()] sets
#' all required parameters at inception, it can be ignored most of the time.
#'
#' It is worthwhile to note that internally, [`Translator`][Translator] objects
#' are collections of [`Block`][Block] objects. Blocks are similar to
#' Translators, but stores a single source text and their translations. They
#' are also somewhat lower-level, and in typical situations, users do not
#' directly interact with them.
#'
#' ## Translating Text
#'
#' Since it can be detected and processed by [find_source()], it is recommended
#' to use [translate()] at all times. Method
#' [`Translator$translate()`][Translator] is the underlying workhorse function
#' called by the former.
#'
#' @param ... Usage depends on the underlying function.
#'   * Any number of [`Block`][Block] objects, and/or named character
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
#' [translator()] returns an [`R6`][R6::R6] object of class
#' [`Translator`][Translator].
#'
#' [is_block()] returns a logical value.
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
#' ## Create a Translator manually.
#' translator(
#'   id = "test-translator",
#'   en = "English",
#'   fr = "Français",
#'   block(
#'     location("a", 1L, 2L, 3L, 4L),
#'     en = "Hello, world!",
#'     fr = "Bonjour, monde!"),
#'   block(
#'     location("b", 5L, 6L, 7L, 8L),
#'     en = "Farewell, world!",
#'     fr = "Au revoir, monde!"))
#'
#' @include constants.R
#' @rdname class-translator
#' @export
translator <- function(..., id = uuid(), hash_algorithm = get_hash_algorithms()) {
    trans <- Translator$new(id, hash_algorithm)
    dots  <- list(...)
    do.call(trans$set_blocks, dots[vapply_1l(dots, is_block)])
    do.call(trans$set_native_languages, dots[vapply_1l(dots, is.character)])
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
        blocks <- lapply(x$hashes, x$get_block)
        langs  <- lapply(blocks, \(b) to_string(b$languages, last_sep = ", "))

        source_texts <- x$source_texts
        hashes <- names(source_texts)  ## These are already reduced.
        names(source_texts) <- sprintf("%s [%s]", hashes, langs)
    } else {
        source_texts <- .__STR_EMPTY_OBJ
    }

    xlist <- list(
        Identifier     = x$id,
        Algorithm      = x$hash_algorithm,
        Languages      = x$native_languages %??% .__STR_EMPTY_OBJ,
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
Translator <- R6::R6Class("Translator",
    lock_class   = TRUE,
    lock_objects = TRUE,
    cloneable    = FALSE,
    private      = list(
        .id           = .__STR_UNDEFINED,  # See $id
        .hash_algo    = .__STR_UNDEFINED,  # See $hash_algorithm
        .native_langs = NULL,              # See $native_languages, $initialize -> new.env()
        .blocks       = NULL,              # See $intialize -> new.env()

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
            hashes <- names(private$.blocks)
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
                assert_match(value, get_hash_algorithms(),
                    quote_values = TRUE,
                    x_name       = "hash_algorithm")

                # Update hash_algorithm of each Block.
                eapply(private$.blocks, \(blk) blk$hash_algorithm <- value)

                # Update object's hash_algorithm.
                private$.hash_algo <- value

                # Reassign updated Block under new hashes.
                blocks          <- as.list(private$.blocks)
                private$.blocks <- new.env(parent = emptyenv())
                do.call(self$set_blocks, blocks)
            }

            return(private$.hash_algo)
        },

        #' @field hashes A character vector of non-empty and non-[NA][base::NA]
        #'   values, or `NULL`. The set of all `hash` exposed by registered
        #'   [`Block`][Block] objects. If there is none, `hashes` is `NULL`.
        #'   This is a **read-only** field. It is automatically updated
        #'   whenever field `hash_algorithm` is updated.
        hashes = \(value) {
            if (!missing(value)) {
                stops(
                    "'hashes' cannot be overwritten.\n",
                    "Update them by setting 'hash_algorithm', and by setting, or removing 'Block' objects.")
            }

            hashes <- eapply(private$.blocks, `[[`, i = "hash")
            return(unlist(hashes, use.names = FALSE))
        },

        #' @field source_texts A character vector of non-empty and
        #'   non-[NA][base::NA] values, or `NULL`. The set of all
        #'   `source_text` exposed by registered [`Block`][Block]
        #'   objects. If there is none, `source_texts` is `NULL`.
        #'   This is a **read-only** field.
        source_texts = \(value) {
            if (!missing(value)) {
                stops(
                    "'source_texts' cannot be overwritten.\n",
                    "Update them by setting, or removing 'Block' objects.")
            }
            if (!length(private$.blocks)) {
                # Otherwhise, assigning names to NULL
                # would throw a non-semantic error.
                return(NULL)
            }

            texts <- unlist(eapply(private$.blocks, `[[`, i = "source_text"))
            names(texts) <- private$.hash_reduce(names(texts))
            return(texts)
        },

        #' @field languages A character vector of non-empty and
        #'   non-[NA][base::NA] values, or `NULL`. The set of all
        #'   `languages` (codes) exposed by registered [`Block`][Block]
        #'   objects. If there is none, `languages` is `NULL`.
        #'   This is a **read-only** field.
        languages = \(value) {
            if (!missing(value)) {
                stops(
                    "'languages' cannot be overwritten.\n",
                    "Update them by setting, or removing 'Block' objects.")
            }

            langs <- eapply(private$.blocks, `[[`, i = "languages")
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
        initialize = \(id = uuid(), hash_algorithm = get_hash_algorithms()) {
            assert_chr1(id)
            assert_arg(hash_algorithm, TRUE)

            private$.id           <- id
            private$.hash_algo    <- hash_algorithm
            private$.native_langs <- new.env(parent = emptyenv())
            private$.blocks       <- new.env(parent = emptyenv())
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
        translate = \(..., lang = "", concat = " ", source_lang = "en") {
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
            if (is.null(blk <- self$get_block(hash))) {
                return(NULL)
            }

            return(blk$get_translation(lang))
        },

        #' @description Extract a [`Block`][Block] object.
        #'
        #' @return A [`Block`][Block] object, or `NULL`.
        get_block = \(hash = "") {
            assert_chr1(hash)
            return(private$.blocks[[private$.hash_expand(hash)]])
        },

        #' @description Simultaneously create and register a [`Block`][Block]
        #'   object.
        #'
        #' @param ... Passed as is to [block()].
        #'
        #' @param source_lang Passed as is to [block()].
        #'
        #' @return A `NULL`, invisibly.
        set_block = \(..., source_lang = source_language_get()) {
            blk <- block(..., source_lang, hash_algorithm = private$.hash_algo)
            self$set_blocks(blk)
            return(invisible())
        },

        #' @description Register one or more [`Block`][Block] objects.
        #'
        #' @param ... Any number of [`Block`][Block] objects.
        #'
        #' @details This method calls [merge_blocks()] to merge all
        #'   values passed to `...` together with previously registered
        #'   [`Block`][Block] objects. The underlying registered source
        #'   texts, translations, and [`Location`][Location] objects
        #'   won't be duplicated.
        #'
        #' @return A `NULL`, invisibly.
        #'
        #' @examples
        #' ## Create Block objects.
        #' blk1 <- block(
        #'   location("a", 1L, 2L, 3L, 4L),
        #'   en = "Hello, world!",
        #'   fr = "Bonjour, monde!")
        #' blk2 <- block(
        #'   location("b", 5L, 6L, 7L, 8L),
        #'   en = "Farewell, world!",
        #'   fr = "Au revoir, monde!")
        #'
        #' ## Create a new Translator and register them.
        #' trans <- Translator$new()
        #' trans$set_blocks(blk1, blk2)
        set_blocks = \(...) {
            if (!...length()) {
                return(invisible())
            }

            args   <- c(as.list(private$.blocks), list(...), hash_algorithm = private$.hash_algo)
            blocks <- do.call(merge_blocks, args)
            names(blocks) <- vapply_1c(blocks, `[[`, i = "hash")

            list2env(blocks, private$.blocks)
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
        #'   identifying the [`Block`][Block] object to be removed.
        #'
        #' @return A `NULL`, invisibly.
        rm_block = \(hash = "") {
            if (!length(private$.blocks)) {
                stops("there are no registered 'Block' objects to remove.")
            }

            assert_chr1(hash)
            assert_match(hash,
                private$.hash_reduce(self$hashes),
                quote_values = TRUE)

            rm(list = private$.hash_expand(hash), envir = private$.blocks)
            return(invisible())
        }
    )
)
