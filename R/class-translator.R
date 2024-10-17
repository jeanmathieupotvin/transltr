#' Placeholder
#'
#' Placeholder for future class [`Translator`][Translator].
#'
#' @rdname class-translator
#' @export
translator <- function(..., hash_algorithm = get_hash_algorithms()) {
    trans <- Translator$new(hash_algorithm)
    trans$set_blocks(...)
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
    xlist <- list(
        Identifier = x$id,
        Language   = x$language,
        Algorithm  = x$hash_algorithm,
        Keys       = x$keys_map %??% "<none>",
        Hashes     = x$hashes   %??% "<none>")

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
        .language  = "<unset>",  # See $language
        .hash_algo = "<unset>",  # See $hash_algorithm
        .keys_map  = NULL,       # See $keys_map, $initialize -> new.env()
        .blocks    = NULL        # See $intialize -> new.env()
    ),
    active = list(
        language = \(value) {
            if (!missing(value)) {
                assert_chr1(value)
                assert_match(value, keys, quote_values = TRUE)
                private$.language <- value
            }

            return(private$.language)
        },
        hashes = \(value) {
            if (!missing(value)) {
                stops(
                    "'hashes' cannot be manually overwritten.\n",
                    "You may add a hash with method 'set_block()'.\n",
                    "You may remove a hash with method 'rm_block()'.")
            }

            hashes <- eapply(private$.blocks, `[[`, i = "hash", USE.NAMES = FALSE)
            return(sort(unlist(hashes)))
        },
        hash_algorithm = \(value) {
            if (!missing(value)) {
                private$.hash_algo <- value
                eapply(private$.blocks, \(blk) blk$hash_algorithm <- value)
            }

            return(private$.hash_algo)
        },
        source_texts = \(value) {
            if (!missing(value)) {
                stops(
                    "'source_texts' cannot be manually overwritten.\n",
                    "You may add a source text with method 'set_block()'.\n",
                    "You may remove a source text with method 'rm_block()'.")
            }

            texts <- eapply(private$.blocks, `[[`, i = "source_text")
            return(sort(unlist(texts)))
        },
        keys = \(value) {
            if (!missing(value)) {
                stops(
                    "'keys' cannot be manually overwritten.\n",
                    "You may add key(s) with method 'set_block()'.\n",
                    "You may remove key(s) with method 'rm_block()'.")
            }

            keys <- eapply(private$.blocks, `[[`, i = "keys")
            return(sort(unique(unlist(keys))))
        },
        keys_map = \(value) {
            if (!missing(value)) {
                stops(
                    "'keys_map' cannot be manually overwritten.\n",
                    "You may add and/or remove entries to the map with method 'map_key()'.")
            }

            return(unlist(as.list(private$.keys_map, sorted = TRUE)))
        }
    ),
    public = list(
        initialize = \(hash_algorithm = get_hash_algorithms()) {
            assert_arg(hash_algorithm)
            private$.blocks   <- new.env(parent = emptyenv())
            private$.keys_map <- new.env(parent = emptyenv())
            return(self)
        },
        translate = \(..., concat = " ", source_key = "") {
            assert_chr1(concat)
            assert_chr1(source_key)
            text <- text_normalize(..., concat)
            hash <- text_hash(source_key, text, private$.hash_algorithm)
            return(self$get_translation(hash, private$.language))
        },
        get_translation = \(hash = "", key = "") {
            return(self$get_block(hash)$get_translation(key))
        },
        get_block = \(hash = "") {
            assert_chr1(hash)
            return(private$.blocks[[hash]])
        },
        set_block = \(source_key = "", ...) {
            blk <- block(source_key, ..., hash_algorithm = private$.hash_algo)
            private$.blocks[[blk$hash]] <- blk
            return(invisible(TRUE))
        },
        set_blocks = \(...) {
            if (!...length()) {
                return(invisible(TRUE))
            }
            if (!all(vapply_1l(blocks <- list(...), is_block))) {
                stops("values passed to '...' must all be 'Block' objects.")
            }

            names(blocks) <- vapply_1c(blocks, `[[`, i = "hash")
            list2env(trans, envir = private$.blocks)
            return(invisible(TRUE))
        },
        map_key = function(...) {
            if (!...length()) {
                return(invisible(TRUE))
            }
            if (!all(vapply_1l(map <- list(...), \(x) is_chr1(x) || is.null(x)))) {
               stops(
                "values passed to '...' must all be character strings or NULL.\n",
                "Use NULL to remove entries from the map.")
            }

            assert_named(map, x_name = "...")
            list2env(map, private$.keys_map)
            return(invisible(TRUE))
        },
        rm_block = \(hash = "") {
            assert_chr1(hash)
            assert_match(hash, self$hashes, quote_values = TRUE)
            rm(list = hash, envir = private$.blocks)
            return(invisible(TRUE))
        }
    )
)
