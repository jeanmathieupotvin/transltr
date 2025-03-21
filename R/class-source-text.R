#' @export
SourceText <- R6::R6Class("SourceText",
    lock_class   = TRUE,
    lock_objects = TRUE,
    cloneable    = TRUE,
    private      = list(
        .id = "",
        .source_lang = "",
        .source_text = "",
        .source_text_hash = "",
        .translations = NULL,
        .source_locations = NULL,
        .strings = NULL
    ),
    active = list(
        id = \(value) {
            if (!missing(value)) {
                stops("'$id' is a read-only value.")
            }

            return(private$.id)
        },

        source_lang = \(value) {
            if (!missing(value)) {
                stops("'$source_lang' is a read-only value.")
            }

            return(private$.source_lang)
        },

        source_text = \(value) {
            if (!missing(value)) {
                stops("'$source_text' is a read-only value.")
            }

            return(private$.source_text)
        },

        source_text_hash = \(value) {
            if (!missing(value)) {
                stops("'$source_text_hash' is a read-only value.")
            }

            return(private$.source_text_hash)
        },

        languages = \(value) {
            if (!missing(value)) {
                stops("'$languages' is a read-only value.")
            }

            return(c(self$source_lang, sort(names(self$translations))))
        },

        translations = \(value) {
            if (!missing(value)) {
                stops("'$translations' is a read-only value.")
            }

            return(
                unlist(
                    as.list(
                        private$.translations,
                        all.names = TRUE,
                        sorted    = TRUE)) %??% character())
        },

        source_locations = \(value) {
            if (!missing(value)) {
                stops("'$source_locations' is a read-only value.")
            }

            return(
                as.list(
                     private$.source_locations,
                     all.names = TRUE,
                     sorted    = TRUE))
        }
    ),
    public = list(
        initialize = \(..., source_lang = language_source_get()) {
            assert_chr1(source_lang)

            source_text <- normalize(...)

            private$.id <- hash_dots(...)
            private$.source_lang <- source_lang
            private$.source_text <- source_text
            private$.source_text_hash <- hash_str(source_text)

            private$.translations <- new.env(parent = emptyenv())
            private$.source_locations <- new.env(parent = emptyenv())
            private$.strings <- new.env(parent = emptyenv())

            assign(source_lang, source_text, private$.strings)
            return(self)
        },

        get_translation = \(lang = "") {
            assert_chr1(lang)
            return(self$get_translation_unsafe(lang))
        },

        get_translation_unsafe = \(lang) {
            return(private$.strings[[lang]])
        },

        add_translation = \(lang = "", ...) {
            assert_chr1(lang)

            if (!is.character(c(...))) {
                stops("values passed to '...' must all be character vector.")
            }

            trans <- normalize(...)

            assign(lang, trans, private$.translations)
            assign(lang, trans, private$.strings)
            return(invisible(self))
        },

        rm_translation = \(lang = "") {
            assert_chr1(lang)
            assert_match(lang,
                choices      = names(private$.translations),
                quote_values = TRUE)

            rm(list = lang, pos = private$.translations)
            rm(list = lang, pos = private$.strings)
            return(invisible(self))
        },

        add_source_locations = \(..., .list = list()) {
            if (!...length() && !length(.list)) {
                return(invisible(self))
            }

            assert_list(.list, TRUE)

            locs <- c(self$source_locations, list(...), .list)
            locs <- do.call(merge_source_locations, locs)

            list2env(locs, private$.source_locations)
            return(invisible(self))
        },

        rm_source_location = \(id = "") {
            assert_chr1(id)
            assert_match(id,
                choices      = names(private$.source_locations),
                quote_values = TRUE)

            rm(list = id, pos = private$.source_locations)
            return(invisible(self))
        },
        as_list = \(coerce_locations = TRUE, escape_newlines = FALSE, ...) {
            assert_lgl1(coerce_locations)
            assert_lgl1(escape_newlines)

            out <- list(
                id               = self$id,
                source_lang      = self$source_lang,
                source_text      = self$source_text,
                source_text_hash = self$source_text_hash,
                translations     = self$translations,
                source_locations = self$source_locations)

            if (escape_newlines) {
                out$source_text  <- escape_std_paragraph_sep(out$source_text)
                out$translations <- escape_std_paragraph_sep(out$translations)
            }

            if (coerce_locations) {
                out$source_locations <- lapply(out$source_locations, as.list, ...)
            }

            return(out)
        }
    )
)

#' @export
source_text <- function(..., source_lang = language_source_get()) {
    dots <- list(...)
    dots_chars <- unlist(dots[vapply_1l(dots, is.character)])

    text <- SourceText$new(dots_chars, source_lang = source_lang)
    text$add_source_locations(.list = dots[vapply_1l(dots, is_source_location)])
    return(text)
}

#' @export
is_source_text <- function(x) {
    return(inherits(x, "SourceText"))
}

#' @export
format.SourceText <- function(x, indent = 1L, ...) {
    out <- x$as_list(FALSE, TRUE, ...)

    # Format source locations and inject the resulting
    # character into the field. It is indented properly
    # below. Setting names equal to :: is just a clever
    # way of introducing separators between elements.
    # Since names are equal to $identifiers, it is not
    # relevant (and confusing) to show them twice.
    out$source_locations <- structure(
        lapply(out$source_locations, format, indent = 0L, ...),
        names = rep.int("::", length(out$source_locations)))

    return(format_vector(out, level = indent, indent = 1L))
}

#' @export
as.list.SourceText <- function(x, ...) {
    return(x$as_list(...))
}

#' @export
print.SourceText <- function(x, ...) {
    cat("<SourceText>", format(x, ...), sep = "\n")
    return(invisible(x))
}

#' @export
c.SourceText <- function(...) {
    if (...length() < 2L) {
        return(..1)
    }
    if (!all(vapply_1l(texts <- list(...), is_source_text))) {
        stops("values passed to '...' must all be 'SourceText' objects.")
    }

    ids <- vapply_1c(texts, `[[`, i = "id")

    # Checking hashes simultaneously checks
    # equality of source_lang and source_text.
    if (!all(ids[[1L]] == ids[-1L])) {
        stops("all '$id' must be equal to combine 'SourceText' objects.")
    }

    # Names of elements passed to ... are stripped
    # to preserve original language codes stemming
    # from all SourceTexts and merged with unlist()
    # below.
    names(texts) <- NULL

    # unlist(, FALSE) avoids nested lists
    # when extracting all source_locations.
    locs  <- unlist(lapply(texts, `[[`, i = "source_locations"), FALSE)
    trans <- unlist(lapply(texts, `[[`, i = "translations"))

    text <- ..1$clone(deep = TRUE)
    map(text$add_translation, lang = names(trans), trans)
    text$add_source_locations(.list = locs)

    return(text)
}

#' @export
merge_source_texts <- function(...) {
    if (!all(vapply_1l(texts <- list(...), is_source_text))) {
        stops("values passed to '...' must all be 'SourceText' objects.")
    }

    ids <- vapply_1c(texts, `[[`, i = "id")
    groups <- unname(split(texts, ids))
    merged <- lapply(groups, \(group) do.call(c, group))
    names(merged) <- vapply_1c(merged, `[[`, i = "id")
    return(merged)
}

#' @export
as_source_text <- function(x, ...) {
    UseMethod("as_source_text")
}

#' @export
as_source_text.call <- function(x, loc = source_location(), ...) {
    if (!is_source_location(loc)) {
        stops("'loc' must be a 'SourceLocation' object.")
    }

    x <- match.call(
        call        = x,
        definition  = SourceText$public_methods$initialize,
        expand.dots = FALSE)

    dots <- x$`...` %??% ""
    source_lang <- x$source_lang %??% language_source_get()

    text <- SourceText$new(dots, source_lang = source_lang)
    text$add_source_locations(loc)
    return(text)
}

#' @export
str_to.SourceText <- function(x, indent = 0L, ...) {
    str <- sprintf("%s [%s] %s",
        x$id,
        str_to(x$languages, last_sep = ", "),
        escape_std_paragraph_sep(x$source_text))

    return(format_vector(str, level = indent, indent = 1L))
}
