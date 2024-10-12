#' Translations Blocks
#'
#' Structure and manipulate source texts and their translations.
#'
#' A [`Block`][Block] is first and foremost a collection of translations of
#' a given source text (i.e. text extracted from \R source scripts). These
#' translations are each identified by a *language key*, a character string
#' that can be mapped to a native language. For more information, see section
#' *[Language keys][Translations Source Files]*.
#'
#' It has two further components.
#'
#'   1. A repducible hash derived from an hashing algorithm that can be used
#'   to identify the source key and text.
#'
#'   2. One or more source locations. See class [`Location`][Location] for more
#'   information.
#'
#' Users should never attempt to manipulate the information contained in a
#' [`Block`][Block]. The latter exposes an API to safely do so (see below),
#' but it is meant to be used internally. Consider using exported features
#' instead.
#'
#' Users may create [`Block`][Block] objects on-the-fly for testing purposes
#' with [block()]. [.block()] should be reserved for internal use-cases.
#'
#' @param source_key A non-empty and non-[NA][base::NA] character string. The
#'   (default) language key of `source_text`. See Details for more information.
#'
#' @param source_text A non-empty and non-[NA][base::NA] character string.
#'
#' @param trans_keys A character vector of non-empty and non-[NA][base::NA]
#'   values. The language keys identifying elements of `trans_texts`.
#'
#' @param trans_texts A character vector of of non-empty and non-[NA][base::NA]
#'   values. Translations of `source_text`.
#'
#' @param locations A list of [`Location`][Location] objects or a single
#'   [`Location`][Location] object.
#'
#' @param x Any \R object. A [`Block`][Block] object for [format()] and
#'   [print()].
#'
#' @param ... Usage depends on the underlying function.
#'   * Any number of [`Location`][Location] objects and/or named character
#'     strings for [block()] (in no preferred order).
#'   * Any number of [`Block`][Block] objects for [merge_blocks()] and S3
#'     method [c()].
#'   * Further arguments passed to or from other methods for [format()],
#'     [print()], and [as_block()].
#'
#' @template param-hash-algorithm
#'
#' @returns
#' [block()] and [.block()] return an [`R6`][R6::R6] object of class
#' [`Block`][Block].
#'
#' [is_block()] returns a logical.
#'
#' [format()] returns a character.
#'
#' [print()] returns argument `x` invisibly.
#'
#' [c()] returns a [`Block`][Block] object. It can only combine objects
#' having the exact same `hash`, which is equivalent to having the same
#' `hash_algorithm`, `source_key`, and `source_text`. In that case, the
#' underlying translations and locations are combined into coherent sets.
#'
#' [merge_blocks()] returns a list of [`Block`][Block] objects. It is
#' a generalized version of [c()] that handles [`Block`][Block] objects
#' having different hash(es).
#'
#' [as_block()] returns a [`Block`][Block] object.
#'
#' @examples
#' ## Create a Block object.
#' transltr:::block("en",
#'     transltr:::location("a", 1L, 2L, 3L, 4L),
#'     transltr:::location("a", 1L, 2L, 3L, 4L),
#'     transltr:::location("b", 5L, 6L, 7L, 8L),
#'     transltr:::location("c", c(9L, 10L), c(11L, 12L), c(13L, 14L), c(15L, 16L)),
#'     en = "Hello, world!",
#'     fr = "Bonjour, monde!",
#'     es = "¡Hola Mundo!",
#'     jp = "こんにちは世界！")
#'
#' ## Combine Blocks objects.
#' b1 <- transltr:::block("en",
#'     transltr:::location("a", 1L, 2L, 3L, 4L),
#'     en = "Hello, world!",
#'     fr = "Bonjour, monde!",
#'     es = "¡Hola Mundo!",
#'     jp = "こんにちは世界！")
#'
#' b2 <- transltr:::block("en",
#'     transltr:::location("a", 5L, 6L, 7L, 8L),
#'     en     = "Hello, world!",
#'     fr     = "Bonjour, monde!",
#'     es     = "¡Hola Mundo!",
#'     `jp-2` = "こんにちは世界！")
#'
#' b3 <- transltr:::block("fr",
#'     transltr:::location("c", 1L, 2L, 3L, 4L),
#'     en     = "Hello, world!",
#'     fr     = "Bonjour, monde!",
#'     `es-2` = "¡Hola Mundo!",
#'     `jp-2` = "こんにちは世界！")
#'
#' c(b1, b2)
#' transltr:::merge_blocks(b1, b2, b3)
#'
#' @rdname class-block
#' @keywords internal
block <- function(source_key = "", ..., hash_algorithm = get_hash_algorithms()) {
    blk  <- Block$new(hash_algorithm)
    dots <- list(...)
    do.call(blk$set_translations, dots[vapply_1l(dots, is.character)])
    do.call(blk$set_locations,    dots[vapply_1l(dots, is_location)])
    blk$source_key <- source_key
    return(blk)
}

#' @usage
#' ## Internal constructor
#' .block(
#'   source_key     = "",
#'   source_text    = "",
#'   hash_algorithm = get_hash_algorithms(),
#'   trans_keys     = character(),
#'   trans_texts    = character(),
#'   locations      = list()
#' )
#' @rdname class-block
#' @keywords internal
.block <- function(
    source_key     = "",
    source_text    = "",
    hash_algorithm = get_hash_algorithms(),
    trans_keys     = character(),
    trans_texts    = character(),
    locations      = list())
{
    if (length(trans_keys) != length(trans_texts)) {
        stops("'trans_keys' and 'trans_texts' must have the same length.")
    }

    # What locations contains is checked by $set_locations.
    # Here, we ensure that it is a list possibly containing
    # Location objects. This is required by do.call() below.
    if (is_location(locations) || !is.list(locations)) {
        locations <- list(locations)
    }

    blk <- Block$new(hash_algorithm)
    blk$set_translation(source_key, source_text)
    blk$source_key <- source_key

    do.call(blk$set_locations, locations)
    .mapply(blk$set_translation, list(trans_keys, trans_texts), list())
    return(blk)
}

#' @rdname class-block
#' @keywords internal
is_block <- function(x) {
    return(inherits(x, "Block"))
}

#' @rdname class-block
#' @export
format.Block <- function(x, ...) {
    # For translations and locations, we print
    # FIELD: <none> if either field is empty.

    trans_strs <- if (length(trans <- x$translations)) {
        # We want a total width of 80 chars, ignoring
        # non-ASCII chars that may have width > 1.
        # This yields 80 chars
        #     minus 4 spaces for indentation
        #     minus X spaces for padded keys
        #     minus 2 chars for the separator (': ').
        keys  <- left_pad_strings(names(trans))
        trans <- trim_strings(trans, 74L - max(nchar(keys), 0L))
        c("  Translations: ", sprintf("    %s: %s", keys, trans))
    } else {
        c("  Translations: " = "<none>")
    }

    locs_strs <- if (length(locs <- x$locations)) {
        c("  Locations: ", sprintf("    %s", unlist(lapply(locs, format))))
    } else {
        c("  Locations: " = "<none>")
    }

    x_str <- c(
        "<Block>",
        "  Hash      : " = x$hash,
        "  Algorithm : " = x$hash_algorithm,
        "  Source Key: " = x$source_key,
        "  ------------------------------------------------------------------------------",
        trans_strs,
        "  ------------------------------------------------------------------------------",
        locs_strs)

    return(paste0(names(x_str), x_str))
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
    # of hash_algorithm, source_key and source_text.
    if (!all(hashes[[1L]] == hashes[-1L])) {
        stops("all 'hash' must be equal in order to combine multiple 'Block' objects.")
    }

    trans <- unlist(lapply(blocks, `[[`, i = "translations"))
    locs  <- unlist(lapply(blocks, `[[`, i = "locations"), FALSE)
    blk   <- .block(..1$source_key, ..1$source_text, ..1$hash_algorithm)
    do.call(blk$set_translations, as.list(trans))
    do.call(blk$set_locations, locs)
    return(blk)
}

#' @rdname class-block
#' @keywords internal
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
as_block <- function(x, ...) {
    UseMethod("as_block")
}

#' @rdname class-block
#' @export
as_block.call <- function(x,
    locations      = list(),
    hash_algorithm = get_hash_algorithms(),
    ...)
{
    # FIXME: this function may call as_block.character() in
    # a future iteration. This would simplify its signature.
    suppressWarnings(strings <- as.character(x$`...`))

    if (!is_chr1(x$key) || !is_chr1(x$concat) || !is.character(strings)) {
        stops(
            "Values passed to 'key' and 'concat' must be non-empty literal character strings.\n",
            "Values passed to '...' must all be literal character strings. They can be empty.\n",
            "Otherwise, they cannot be safely evaluated before runtime.\n",
            "Check the following source location(s).\n",
            unlist(lapply(locations, format)))
    }

    return(
        .block(
            x$key,
            sanitize_strings(strings, x$concat),
            hash_algorithm,
            locations = locations))
}

#' @rdname class-block
#' @export
as_block.character <- function(x,
    source_key     = "",
    locations      = list(),
    hash_algorithm = get_hash_algorithms(),
    ...)
{
    # FIXME: this is a placeholder for future purposes.
    # I am unusure whether it will be useful or not.
    return(.NotYetUsed())
}

#' @rdname class-block
#' @importFrom digest sha1
#' @keywords internal
Block <- R6::R6Class("Block",
    lock_class   = TRUE,
    lock_objects = TRUE,
    cloneable    = FALSE,
    private      = list(
        .hash         = "<unset>",  # See $hash
        .hash_algo    = "<unset>",  # See $hash_algorithm
        .source_key   = "<unset>",  # See $source_key
        .translations = NULL,       # See $translations
        .locations    = list(),     # See $locations

        # @description Compute hash of the key/text pair.
        #
        # @param key A non-empty and non-NA character string. The language key.
        # @param text A non-empty and non-NA character string. The source text
        #   or one of its translations.
        #
        # @return A character string.
        .hash_do = \(key = "", text = "") {
            x <- sprintf("%s:%s", key, text)
            return(
                switch(private$.hash_algo,
                    sha1 = digest::sha1(charToRaw(x)),
                    utf8 = as.character(sum(cumsum(utf8ToInt(x)))),
                    NULL))
        }
    ),
    active = list(
        #' @field hash A non-empty and non-[NA][base::NA] character string. A
        #'   reproducible hash generated from `source_key` and `source_text`
        #'   using the algorithm given by `hash_algorithm`. It is used as a
        #'   unique identifier for the [`Block`][Block] object.
        #'
        #'   This is a **read-only** field. It is automatically updated whenever
        #'   fields `source_key` and/or `hash_algorithm` are updated.
        hash = \(value) {
            if (!missing(value)) {
                stops("'hash' cannot be manually overwritten. Set 'source_key' instead.")
            }

            return(private$.hash)
        },

        #' @field hash_algorithm A non-empty and non-[NA][base::NA] character
        #'   string. The algorithm to use when hashing source information for
        #'   identification purposes. It must be a value returned by
        #'   [get_hash_algorithms()].
        hash_algorithm = \(value) {
            if (!missing(value)) {
                assert_chr1(value)
                assert_match(value,
                    choices      = get_hash_algorithms(),
                    quote_values = TRUE,
                    x_name       = "hash_algorithm")

                key <- private$.source_key
                private$.hash_algo <- value
                private$.hash      <- private$.hash_do(key, self$get_translation(key))
            }

            return(private$.hash_algo)
        },

        #' @field source_key A non-empty and non-[NA][base::NA] character
        #'   string. The language key of `source_text`.
        source_key = \(value) {
            if (!missing(value)) {
                assert_chr1(value)
                assert_match(value, self$keys,
                    quote_values = TRUE,
                    x_name       = "source_key")

                private$.source_key <- value
                private$.hash       <- private$.hash_do(value, self$get_translation(value))
            }

            return(private$.source_key)
        },

        #' @field source_text A non-empty and non-[NA][base::NA] character
        #'   string. The source text to be translated. This is a **read-only**
        #'   field.
        source_text = \(value) {
            if (!missing(value)) {
                stops("'source_text' cannot be manually overwritten. Set 'source_key' instead.")
            }

            return(self$get_translation(private$.source_key))
        },

        #' @field keys A character vector. Registered language keys. This is a
        #'   **read-only** field.
        keys = \(value) {
            if (!missing(value)) {
                stops(
                    "'keys' cannot be manually overwritten.\n",
                    "You may add a key with method 'set_translation()'.\n",
                    "You may remove a key with method 'rm_translation()'.")
            }

            keys <- sort(names(private$.translations))
            attr(keys, "source_key") <- private$.source_key
            return(keys)
        },

        #' @field translations A non-empty named character vector of
        #'   non-[NA][base::NA] values. Registered translations of
        #'   `source_text`. Names correspond to the underlying language
        #'   `keys`. This is a **read-only** field.
        translations = \(value) {
            if (!missing(value)) {
                stops(
                    "'translations' cannot be manually overwritten.\n",
                    "You may add a translation with method 'set_translation()'.\n",
                    "You may remove a translation with method 'rm_translation()'.")
            }

            translations <- as.list(private$.translations, sorted = TRUE)
            storage.mode(translations) <- "character"
            return(translations)
        },

        #' @field locations A list of [`Location`][Location] objects giving
        #'   the location(s) of `source_text` in the underlying project. This
        #'   is a **read-only** field.
        locations = \(value) {
            if (!missing(value)) {
                stops(
                    "You may add a location with method 'set_location()'.\n",
                    "You may remove a location with method 'rm_location()'.")
            }

            return(private$.locations)
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
            return(self)
        },

        #' @description Extract a translation.
        #'
        #' @details This method can also be used to extract `source_text`.
        #'
        #  Package roxygen2 7.3.2 automatically reuses templates whenever
        #  it is appropriate within an R6 class.
        #' @template param-key
        #'
        #' @return A character string. `NULL` is returned if `key` is not
        #'   registered.
        get_translation = \(key = "") {
            assert_chr1(key)
            return(private$.translations[[key]])
        },

        #' @description Register a translation.
        #'
        #' @details This method is also used to register `source_key` and
        #'  `source_text` **before** setting them as such. See Examples below.
        #'
        #' @param text A non-empty and non-[NA][base::NA] character string. A
        #'   translation (or a source text).
        #'
        #' @return A `TRUE` (invisibly).
        #'
        #' @examples
        #' ## Registering source_key and source_text.
        #' blk <- transltr:::Block$new()
        #' blk$set_translation("en", "Hello, world!")
        #' blk$source_key <- "en"
        set_translation = \(key = "", text = "") {
            assert_chr1(key)
            assert_chr1(text, TRUE)
            private$.translations[[key]] <- text
            return(invisible(TRUE))
        },

        #' @description Register one or more translations.
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
        #' blk <- transltr:::Block$new()
        #' blk$set_translations(en = "Hello, world!", fr = "Bonjour, monde!")
        set_translations = \(...) {
            if (!...length()) {
                return(invisible(TRUE))
            }
            if (!all(vapply_1l(trans <- list(...), is_chr1))) {
                stops("values passed to '...' must all be character strings.")
            }

            assert_named(trans, x_name = "...")
            list2env(trans, envir = private$.translations)
            return(invisible(TRUE))
        },

        #' @description Register one or more locations.
        #'
        #' @param ... Any number of [`Location`][Location] objects.
        #'
        #' @return A `TRUE` (invisibly).
        set_locations = \(...) {
            if (!...length()) {
                return(invisible(TRUE))
            }

            # c() dispatches on c.default() to construct
            # a new list, not to c.Location(). locs is a
            # list of Location objects.
            locs <- c(private$.locations, list(...))
            private$.locations <- do.call(merge_locations, locs)
            return(invisible(TRUE))
        },

        #' @description Remove a registered translation.
        #'
        #' @details You cannot remove a `key` registered as `source_key`.
        #'   You must assign a new value as `source_key` before doing so.
        #'
        #' @return A logical (invisibly) indicating whether the operation
        #'   succeeded or not.
        #'
        #' @examples
        #' ## Removing source_key and source_text.
        #' blk <- transltr:::Block$new()
        #' blk$set_translations(en = "Hello, world!", fr = "Bonjour, monde!")
        #' blk$source_key <- "en"
        #'
        #' blk$source_key <- "fr"
        #' blk$rm_translation("en")
        rm_translation = \(key = "") {
            assert_chr1(key)

            if (key == private$.source_key) {
                stopf(
                    "'key' '%s' is the current 'source_key'. %s",
                    "Set a new one before removing it.", key)
            }

            keys <- self$keys
            assert_match(key, keys[keys != self$source_key], quote_values = TRUE)

            rm(list = key, envir = private$.translations)
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
            paths <- vapply_1c(private$.locations, `[[`, i = "path")
            assert_match(path, paths, quote_values = TRUE)
            private$.locations[paths == path] <- NULL
            return(invisible(TRUE))
        }
    )
)
