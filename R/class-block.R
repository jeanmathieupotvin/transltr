#' Source Text and Translations
#'
#' Store, structure, and manipulate **a single** source text and its
#' translations.
#'
#' A [`Block`][Block] object is a collection of translations of a *source text*
#' (i.e. text extracted from \R source scripts). It exposes a set of methods
#' that can be used to safely manipulate the information it contains, but it
#' is unlikely to be useful in typical circumstances. See class
#' [`Translator`][Translator] instead.
#'
#' ## Combining Block Objects
#'
#' [c()] can only combine [`Block`][Block] objects having the same `hash`.
#' This is equivalent to having the same `hash_algorithm`, `source_lang`,
#' and `source_text`. In that case, the underlying translations and
#' [`Location`][Location] objects are combined, and a new object is returned.
#'
#' [merge_blocks()] is a generalized version of [c()] that handles any number
#' of [`Block`][Block] objects having possibly different hashes. It can be
#' viewed as a vectorized version of [c()].
#'
#' @param x Any \R object.
#'
#' @param ... Usage depends on the underlying function.
#'   * Any number of [`Location`][Location] objects, and/or named character
#'     strings for [block()] (in no preferred order).
#'   * Any number of [`Block`][Block] objects for [merge_blocks()] and S3
#'     method [c()].
#'   * Further arguments passed to or from other methods for [format()],
#'     [print()], and [as_block()].
#'
#' @param location A [`Location`][Location] object.
#'
#' @template param-source-lang
#'
#' @template param-hash-algorithm
#'
#' @returns
#' [block()], [c()], and [as_block()] return an [`R6`][R6::R6] object of
#' class [`Block`][Block].
#'
#' [is_block()] returns a logical value.
#'
#' [format()] returns a character vector.
#'
#' [print()] returns argument `x` invisibly.
#'
#' [merge_blocks()] returns a list of (combined) [`Block`][Block] objects.
#'
#' @examples
#' ## Create a Block object.
#' block("en",
#'     location("a", 1L, 2L, 3L, 4L),
#'     location("a", 1L, 2L, 3L, 4L),
#'     location("b", 5L, 6L, 7L, 8L),
#'     location("c", c(9L, 10L), c(11L, 12L), c(13L, 14L), c(15L, 16L)),
#'     en = "Hello, world!",
#'     fr = "Bonjour, monde!",
#'     es = "¡Hola Mundo!",
#'     ja = "こんにちは世界！")
#'
#' ## Combine Blocks objects.
#' b1 <- block("en",
#'     location("a", 1L, 2L, 3L, 4L),
#'     en = "Hello, world!",
#'     fr = "Bonjour, monde!",
#'     es = "¡Hola Mundo!",
#'     ja = "こんにちは世界！")
#'
#' b2 <- block("en",
#'     location("a", 5L, 6L, 7L, 8L),
#'     en     = "Hello, world!",
#'     fr     = "Bonjour, monde!",
#'     es     = "¡Hola Mundo!",
#'     `ja-2` = "こんにちは世界！")
#'
#' b3 <- block("fr",
#'     location("c", 1L, 2L, 3L, 4L),
#'     en     = "Hello, world!",
#'     fr     = "Bonjour, monde!",
#'     `es-2` = "¡Hola Mundo!",
#'     `ja-2` = "こんにちは世界！")
#'
#' c(b1, b2)
#' merge_blocks(b1, b2, b3)
#'
#' @include constants.R
#' @rdname class-block
#' @keywords internal
#' @export
block <- function(source_lang = "", ..., hash_algorithm = get_hash_algorithms()) {
    assert_chr1(source_lang)

    dots  <- list(...)
    texts <- dots[vapply_1l(dots, is.character)]
    locs  <- dots[vapply_1l(dots, is_location)]

    if (!is_match(source_lang, names(texts))) {
        stops(
            "a translation corresponding to 'source_lang' must be passed to '...'.\n",
            "It is treated as the source text.")
    }

    blk <- Block$new(hash_algorithm)
    do.call(blk$set_translations, texts)
    do.call(blk$set_locations, locs)
    blk$source_lang <- source_lang
    return(blk)
}

#' @rdname class-block
#' @keywords internal
#' @export
is_block <- function(x) {
    return(inherits(x, "Block"))
}

#' @rdname class-block
#' @export
format.Block <- function(x, ...) {
    trans <- if (length(x$translations)) x$translations else .__STR_EMPTY_OBJ
    locs  <- unlist(lapply(x$locations, format), TRUE, FALSE) %??% .__STR_EMPTY_OBJ
    xlist <- list(
        Hash          = x$hash,
        `Source Lang` = x$source_lang,
        Algorithm     = x$hash_algorithm,
        Translations  = trans,
        Locations     = locs)

    return(format_vector(xlist, "<Block>", .show_nokey = FALSE))
}

#' @rdname class-block
#' @export
print.Block <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    return(invisible(x))
}

#' @rdname class-block
#' @export
c.Block <- function(...) {
    if (...length() < 2L) {
        return(..1)
    }
    if (!all(vapply_1l(blocks <- list(...), is_block))) {
        stops("values passed to '...' must all be 'Block' objects.")
    }

    hashes <- vapply_1c(blocks, `[[`, i = "hash")

    # Checking hashes simultaneously checks equality
    # of hash_algorithm, source_lang and source_text.
    if (!all(hashes[[1L]] == hashes[-1L])) {
        stops("all 'hash' must be equal in order to combine multiple 'Block' objects.")
    }

    trans <- unlist(lapply(blocks, `[[`, i = "translations"))
    locs  <- unlist(lapply(blocks, `[[`, i = "locations"), FALSE)

    blk <- Block$new(..1$hash_algorithm)
    do.call(blk$set_translations, as.list(trans))
    do.call(blk$set_locations, locs)
    blk$source_lang <- ..1$source_lang
    return(blk)
}

#' @rdname class-block
#' @keywords internal
#' @export
merge_blocks <- function(..., hash_algorithm = get_hash_algorithms()) {
    if (!all(vapply_1l(blocks <- list(...), is_block))) {
        stops("values passed to '...' must all be 'Block' objects.")
    }

    assert_arg(hash_algorithm)
    lapply(blocks, \(blk) blk$hash_algorithm <- hash_algorithm)

    groups <- split_ul(blocks, vapply_1c(blocks, `[[`, i = "hash"))
    return(lapply(groups, \(group) do.call(c, group)))
}

#' @rdname class-block
#' @keywords internal
#' @export
as_block <- function(x, ...) {
    UseMethod("as_block")
}

#' @rdname class-block
#' @export
as_block.call <- function(x,
    location      = list(),
    hash_algorithm = get_hash_algorithms(),
    ...)
{
    # FIXME: this function may call as_block.character() in
    # a future iteration. This would simplify its signature.
    suppressWarnings(strings <- as.character(x$`...`))

    if (!is_chr1(x$source_lang) || !is_chr1(x$concat) || !is.character(strings)) {
        stops(
            "Values passed to 'source_lang' and 'concat' must be non-empty literal character strings.\n",
            "Values passed to '...' must all be literal character strings. They can be empty.\n",
            "Otherwise, they cannot be safely evaluated before runtime.\n",
            "Check the following source location(s).\n",
            unlist(lapply(locations, format)))
    }

    text <- text_normalize(strings, .concat = x$concat)
    blk  <- Block$new(hash_algorithm)
    blk$set_locations(location)
    blk$set_translation(x$source_lang, text)
    blk$source_lang <- x$source_lang
    return(blk)
}

#' @rdname class-block
#' @keywords internal
#' @export
Block <- R6::R6Class("Block",
    lock_class   = TRUE,
    lock_objects = TRUE,
    cloneable    = FALSE,
    private      = list(
        .hash         = .__STR_UNDEFINED,  # See $hash
        .hash_algo    = .__STR_UNDEFINED,  # See $hash_algorithm
        .source_lang  = .__STR_UNDEFINED,  # See $source_lang
        .translations = NULL,              # See $translations
        .locations    = NULL               # See $locations
    ),
    active = list(
        #' @field hash A non-empty and non-[NA][base::NA] character string. A
        #'   reproducible hash generated from `source_lang` and `source_text`,
        #'   and by using the algorithm specified by `hash_algorithm`. It is
        #'   used as a unique identifier for the underlying [`Block`][Block]
        #'   object.
        #'
        #'   This is a **read-only** field. It is automatically updated
        #'   whenever fields `source_lang` and/or `hash_algorithm` are updated.
        hash = \(value) {
            if (!missing(value)) {
                stops(
                    "'hash' cannot be overwritten.\n",
                    "Update it by setting 'source_lang' instead.")
            }

            return(private$.hash)
        },

        #' @template field-hash-algorithm
        hash_algorithm = \(value) {
            if (!missing(value)) {
                assert_chr1(value, x_name = "hash_algorithm")
                assert_match(value, get_hash_algorithms(),
                    quote_values = TRUE,
                    x_name       = "hash_algorithm")

                private$.hash_algo <- value
                private$.hash      <- text_hash(
                    private$.source_lang,
                    self$get_translation(private$.source_lang),
                    private$.hash_algo)
            }

            return(private$.hash_algo)
        },

        #' @template field-source-lang
        source_lang = \(value) {
            if (!missing(value)) {
                assert_chr1(value, x_name = "source_lang")
                assert_match(value, self$languages,
                    quote_values = TRUE,
                    x_name       = "source_lang")

                private$.source_lang <- value
                private$.hash <- text_hash(
                    value,
                    self$get_translation(value),
                    private$.hash_algo)
            }

            return(private$.source_lang)
        },

        #' @template field-source-text
        source_text = \(value) {
            if (!missing(value)) {
                stops(
                    "'source_text' cannot be overwritten.\n",
                    "Update it by setting 'source_lang'.\n",
                    "You may add a new translation before doing so.")
            }

            return(self$get_translation(private$.source_lang))
        },

        #' @field languages A character vector. Registered language
        #'   codes. This is a **read-only** field. Use methods below
        #'   to update it.
        languages = \(value) {
            if (!missing(value)) {
                stops(
                    "'languages' cannot be overwritten.\n",
                    "Update them by setting, or removing translations.")
            }

            return(sort(names(private$.translations)))
        },

        #' @field translations A named character vector. Registered
        #'   translations of `source_text`, including the latter. Names
        #'   correspond to `languages`. This is a **read-only** field.
        #'   Use methods below to update it.
        translations = \(value) {
            if (!missing(value)) {
                stops(
                    "'translations' cannot be overwritten.\n",
                    "Update them by setting, or removing translations.")
            }

            return(unlist(as.list(private$.translations, sorted = TRUE)))
        },

        #' @field locations A list of [`Location`][Location] objects giving
        #'   the location(s) of `source_text` in the underlying project. It
        #'   can be empty. This is a **read-only** field. Use methods below
        #'   to update it.
        locations = \(value) {
            if (!missing(value)) {
                stops(
                    "'locations' cannot be overwritten.\n",
                    "Update them by setting, or removing 'Location' objects.")
            }

            return(as.list(private$.locations, sorted = TRUE))
        }
    ),
    public = list(
        #' @description Create a [`Block`][Block] object.
        #'
        #' @template param-hash-algorithm
        #'
        #' @return An [`R6`][R6::R6] object of class [`Block`][Block].
        initialize = \(hash_algorithm = get_hash_algorithms()) {
            assert_arg(hash_algorithm, TRUE)

            # self$hash_algorithm is not used here
            # because source translation is not set.
            private$.hash_algo    <- hash_algorithm
            private$.translations <- new.env(parent = emptyenv())
            private$.locations    <- new.env(parent = emptyenv())
            return(self)
        },

        #' @description Extract a translation, or the source text.
        #'
        #  NOTE: Package roxygen2 reuses templates whenever within an R6 class.
        #
        #' @template param-lang
        #'
        #' @return A character string. `NULL` is returned if the requested
        #'   translation is not available.
        get_translation = \(lang = "") {
            assert_chr1(lang)
            return(private$.translations[[lang]])
        },

        #' @description Register a translation, or the source text.
        #'
        #' @details This method is also used to register `source_lang` and
        #'  `source_text` **before** setting them as such. See Examples below.
        #'
        #' @param text A non-empty and non-[NA][base::NA] character string. A
        #'   translation, or a source text.
        #'
        #' @return A `TRUE` (invisibly).
        #'
        #' @examples
        #' ## Registering source_lang and source_text.
        #' blk <- Block$new()
        #' blk$set_translation("en", "Hello, world!")
        #' blk$source_lang <- "en"
        set_translation = \(lang = "", text = "") {
            assert_chr1(lang)
            assert_chr1(text, TRUE)
            private$.translations[[lang]] <- text
            return(invisible(TRUE))
        },

        #' @description Register one or more translations, and/or the source
        #'   text.
        #'
        #' @param ... Any number of named, non-empty, and non-[NA][base::NA]
        #'   character strings.
        #'
        #' @details This method can be viewed as a vectorized version of
        #'   method `set_translation()`.
        #'
        #' @return A `TRUE` (invisibly).
        #'
        #' @examples
        #' blk <- Block$new()
        #' blk$set_translations(en = "Hello, world!", fr = "Bonjour, monde!")
        set_translations = \(...) {
            if (!...length()) {
                return(invisible(TRUE))
            }
            if (!all(vapply_1l(trans <- list(...), is_chr1))) {
                stops("values passed to '...' must all be non-NA and non-empty character strings.")
            }

            assert_named(trans, x_name = "...")
            list2env(trans, private$.translations)
            return(invisible(TRUE))
        },

        #' @description Register one or more locations.
        #'
        #' @param ... Any number of [`Location`][Location] objects.
        #'
        #' @details This method calls [merge_locations()] to merge all
        #'   values passed to `...` together with previously registered
        #'   [`Location`][Location] objects. The underlying registered
        #'   paths and/or ranges won't be duplicated.
        #'
        #' @return A `TRUE` (invisibly).
        set_locations = \(...) {
            if (!...length()) {
                return(invisible(TRUE))
            }

            locs <- c(as.list(private$.locations), list(...))
            locs <- do.call(merge_locations, locs)
            names(locs) <- vapply_1c(locs, `[[`, i = "path")

            list2env(locs, private$.locations)
            return(invisible(TRUE))
        },

        #' @description Remove a registered translation.
        #'
        #' @param lang A non-empty and non-[NA][base::NA] character string
        #'   identifying a translation to be removed.
        #'
        #' @details You cannot remove `lang` when it is registered as the
        #'   current `source_lang`. You must update `source_lang` before
        #'   doing so.
        #'
        #' @return A logical (invisibly) indicating whether the operation
        #'   succeeded or not.
        #'
        #' @examples
        #' ## Removing source_lang and source_text.
        #' blk <- Block$new()
        #' blk$set_translations(en = "Hello, world!", fr = "Bonjour, monde!")
        #' blk$source_lang <- "en"
        #'
        #' blk$source_lang <- "fr"
        #' blk$rm_translation("en")
        rm_translation = \(lang = "") {
            assert_chr1(lang)

            if (lang == private$.source_lang) {
                stopf(
                    "'%s' is the current 'source_lang'. %s",
                    lang, "Set a new one before removing it.")
            }

            langs <- self$languages
            assert_match(lang, langs[langs != self$source_lang], quote_values = TRUE)

            rm(list = lang, envir = private$.translations)
            return(invisible(TRUE))
        },

        #' @description Remove a registered location.
        #'
        #' @param path A non-empty and non-[NA][base::NA] character string
        #'   identifying a [`Location`][Location] object to be removed.
        #'
        #' @return A `TRUE` (invisibly).
        rm_location = \(path = "") {
            assert_chr1(path)
            assert_match(path, names(private$.locations), quote_values = TRUE)

            rm(list = path, envir = private$.locations)
            return(invisible(TRUE))
        }
    )
)
