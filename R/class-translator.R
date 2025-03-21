#' @export
Translator <- R6::R6Class("Translator",
    lock_class   = TRUE,
    lock_objects = TRUE,
    cloneable    = TRUE,
    private      = list(
        .id = "",
        .current_lang = NULL,
        .default_value = NULL,
        .language_names = NULL,
        .source_texts = NULL,
        .source_texts_hashes_ids = NULL
    ),
    active = list(
        id = \(value) {
            if (!missing(value)) {
                assert_chr1(value, x_name = "$id")
                private$.id <- value
            }

            return(private$.id)
        },
        current_lang = \(value) {
            if (!missing(value)) {
                if (!is.null(value) && !is_chr1(value)) {
                    stops("'$current_lang' must be 'NULL', or a non-NA and non-empty character of length 1.")
                }

                private$.current_lang <- value
            }

            return(private$.current_lang)
        },
        default_value = \(value) {
            if (!missing(value)) {
                if (!is.null(value) && !is_chr1(value, TRUE)) {
                    stops("'$default_value' must be 'NULL', or a non-NA character of length 1.")
                }

                private$.default_value <- value
            }

            return(private$.default_value)
        },
        language_names = \(value) {
            if (!missing(value)) {
                stops("'$language_names' is a read-only value.")
            }

            return(
                unlist(
                    as.list(
                        private$.language_names,
                        all.names = TRUE,
                        sorted    = TRUE)) %??% character())
        }
    ),
    public = list(
        initialize = \(id = uuid(), default_value = "") {
            self$id <- id
            self$default_value <- default_value
            private$.language_names <- new.env(parent = emptyenv())
            private$.source_texts <- new.env(parent = emptyenv())
            private$.source_texts_hashes_ids <- new.env(parent = emptyenv())
            return(self)
        },
        translate = \(..., source_lang = language_source_get()) {
            # For optimization purposes, this method
            # bypasses other methods of the class.
            # Each microsecond matters, and doing so
            # ensures optimal performance.

            # hash_dots() reconstructs a SourceText$id.
            # It may not exist.
            st <- private$.source_texts[[hash_dots(...)]]

            if (is.null(st)) {
                return(private$.default_value %??% "")
            }

            lang <- private$.current_lang %??% language_get()

            # Setting $default_value equal to NULL is
            # a signal to return the original source
            # text when the underlying translation is
            # unavailable.
            return(
                st$get_translation_unsafe(lang) %??%
                private$.default_value %??%
                st$source_text)
        },
        add_language_names = \(..., .list = list()) {
            if (!...length() && !length(.list)) {
                return(invisible(self))
            }

            assert_list(.list, TRUE)
            assert_named(.list)

            if (!all(vapply_1l(dots <- c(list(...), .list), is_chr1))) {
               stops("values passed to '...' must all be named character strings.")
            }

            assert_named(dots, x_name = "...")

            list2env(dots, private$.language_names)
            return(invisible(self))
        },
        rm_lang_name = \(lang = "") {
            assert_chr1(lang)
            assert_match(lang, names(private$.language_names), quote_values = TRUE)

            rm(list = lang, pos = private$.language_names)
            return(invisible(self))
        },
        add_source_text = \(..., source_lang = language_source_get()) {
            text <- source_text(..., source_lang = source_lang)
            return(self$add_source_texts(text))
        },
        add_source_texts = \(..., .list = list()) {
            if (!...length() && !length(.list)) {
                return(invisible(self))
            }

            assert_list(.list, TRUE)

            args <- c(as.list(private$.source_texts), list(...), .list)
            texts <- do.call(merge_source_texts, args)
            hashes <- structure(
                lapply(texts, `[[`, i = "source_text_hash"),
                names = names(texts))

            list2env(texts, private$.source_texts)
            list2env(hashes, private$.source_texts_hashes_ids)
            return(invisible(self))
        },
        rm_source_text = \(id = NULL, hash = NULL) {
            if (!is.null(id) && !is.null(hash)) {
                stops("use 'id' or 'hash', but not both at the same time.")
            }
            if (!is.null(hash)) {
                id <- self$get_id_by_hash(hash)
            }

            text <- self$get_source_text_by_id(id)

            rm(list = id, pos = private$.source_texts)
            rm(list = id, pos = private$.source_texts_hashes_ids)
            return(invisible(self))
        },
        get_source_text_by_id = \(id = "") {
            assert_chr1(id)
            return(private$.source_texts[[id]])
        },
        get_source_text_by_hash = \(hash = "") {
            return(self$get_source_text_by_id(self$get_id_by_hash(hash)))
        },
        get_id_by_hash = function(hash = "") {
            assert_chr1(hash)

            if (!length(private$.source_texts_hashes_ids)) {
                return("")
            }

            is_match <- eapply(private$.source_texts_hashes_ids, `==`, hash)
            return(names(which.max(is_match)))
        },
        get_hash_by_id = function(id = "") {
            return(private$.source_texts_hashes_ids[[id]] %??% "")
        },
        as_list = \(coerce_source_texts = TRUE, ...) {
            assert_lgl1(coerce_source_texts)

            source_texts <- if (coerce_source_texts) {
                eapply(private$.source_texts, as.list, ...)
            } else {
                # R6 objects have reference semantics. Not
                # returning clones would lead to undefined
                # behavior.
                eapply(private$.source_texts, \(text) text$clone(deep = TRUE))
            }

            return(
                list(
                    id             = self$id,
                    current_lang   = self$current_lang,
                    default_value  = self$default_value,
                    language_names = self$language_names,
                    source_texts   = source_texts[sort(names(source_texts))]))
        }
    )
)

#' @export
translator <- function(..., id = uuid(), default_value = "") {
    dots <- list(...)

    tr <- Translator$new(id, default_value)
    tr$add_source_texts(.list = dots[vapply_1l(dots, is_source_text)])
    tr$add_language_names(.list = dots[vapply_1l(dots, is.character)])
    return(tr)
}

#' @export
is_translator <- function(x) {
    return(inherits(x, "Translator"))
}

#' @export
format.Translator <- function(x, max_n = 10L, indent = 1L, ...) {
    assert_int1(max_n)

    out <- x$as_list(FALSE, ...)

    # Setting max_n = 0 implies that if () returns
    # NULL implicitly. This removes $source_texts
    # from out to avoid showing it.
    out$source_texts <- if (max_n > 0L) {
        n <- length(out$source_texts)

        # Keep at least 0 elements, but return no more
        # than max_n ones. If max_n is greater than the
        # current number of SourceText, return them all.
        n_keep <- max(0L, min(n, max_n))
        footer <- if (n > max_n) {
            sprintf("--- <%i not shown> ---", n - max_n)
        }

        # Subset source texts based on n_keep.
        out$source_texts |>
        _[seq_len(n_keep)] |>
        # Stringify chosen source texts.
        lapply(str_to) |>
        # Remove names to format resulting
        # strings as values with no labels.
        unname() |>
        # Add a footer indicating whether some
        # source texts were omitted or not. It
        # is NULL (and not shown) if they are
        # all shown.
        c(footer)
    }

    return(format_vector(out, level = indent, indent = 1L))
}

#' @export
as.list.Translator <- function(x, ...) {
    return(x$as_list(...))
}

#' @export
print.Translator <- function(x, ...) {
    cat("<Translator>", format(x, ...), sep = "\n")
    return(invisible(x))
}
