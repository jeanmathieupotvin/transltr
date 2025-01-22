#' Source Text
#'
#' Structure a source text and its translations.
#'
#' A [`Text`][Text] object is a piece of text that is extracted from \R source
#' scripts.
#'
#'   * It (typically) has one or more [`Locations`][Location] within a project.
#'   * It is complemented by any number of translations and further attributes.
#'
#' The [`Text`][Text] class structures this information and exposes a set of
#' methods to manipulate it.
#'
#' ## Combining Text Objects
#'
#' [c()] can only combine [`Text`][Text] objects having the same `hash`.
#' This is equivalent to having the same `algorithm`, `source_lang`, and
#' `source_text`. In that case, the underlying translations and
#' [`Location`][Location] objects are combined and a new object is returned.
#' It throws an error if all [`Text`][Text] objects are empty (they have no
#' set `source_lang`).
#'
#' [merge_texts()] is a generalized version of [c()] that handles any number
#' of [`Text`][Text] objects having possibly different hashes. It can be
#' viewed as a vectorized version of [c()]. It silently ignores and drops
#' all empty [`Text`][Text] objects.
#'
#' ## Coercion
#'
#' [as_text()] is an S3 generic function that attempts to coerce its argument
#' into a suitable [`Text`][Text] object. [as_text.call()] is the method used
#' by [find_source()] to coerce a [`call`][call] object to a [`Text`][Text]
#' object. While it *can* be used, it should be avoided most of the time. Users
#' may extend it by defining their own methods.
#'
#' @param x Any \R object.
#'
#' @param ... Usage depends on the underlying function.
#'   * Any number of [`Location`][Location] objects and/or named character
#'     strings for [text()] (in no preferred order).
#'   * Any number of [`Text`][Text] objects for [merge_texts()] and S3
#'     method [c()].
#'   * Further arguments passed to or from other methods for [format()],
#'     [print()], and [as_text()].
#'
#' @param location A [`Location`][Location] object.
#'
#' @template param-source-lang
#'
#' @template param-algorithm
#'
#' @template param-strict
#'
#' @template param-validate
#'
#' @returns
#' [text()], [c()], and [as_text()] return an [`R6`][R6::R6] object of
#' class [`Text`][Text].
#'
#' [is_text()] returns a logical value.
#'
#' [format()] returns a character vector.
#'
#' [print()] returns argument `x` invisibly.
#'
#' [merge_texts()] returns a list of (combined) [`Text`][Text] objects. It
#' can be empty if all underlying [`Text`][Text] objects are empty.
#'
#' @examples
#' # Set source language.
#' language_source_set("en")
#'
#' # Create Text objects.
#' txt1 <- text(
#'   location("a", 1L, 2L, 3L, 4L),
#'   location("a", 1L, 2L, 3L, 4L),
#'   location("b", 5L, 6L, 7L, 8L),
#'   location("c", c(9L, 10L), c(11L, 12L), c(13L, 14L), c(15L, 16L)),
#'   en = "Hello, world!",
#'   fr = "Bonjour, monde!",
#'   es = "¡Hola, mundo!")
#'
#' txt2 <- text(
#'   location("a", 1L, 2L, 3L, 4L),
#'   en = "Hello, world!",
#'   fr = "Bonjour, monde!",
#'   es = "¡Hola, mundo!")
#'
#' txt3 <- text(
#'   source_lang = "fr2",
#'   location("a", 5L, 6L, 7L, 8L),
#'   en  = "Hello, world!",
#'   fr2 = "Bonjour le monde!",
#'   es  = "¡Hola, mundo!")
#'
#' is_text(txt1)
#'
#' # Texts objects has a specific format.
#' # print() calls format() internally, as expected.
#' print(txt1)
#' print(txt2)
#' print(txt3)
#'
#' # Combine Texts objects.
#' # c() throws an error if they do not have the same
#' # hash (same souce_text, source_lang, and algorithm).
#' c(txt1, txt2)
#'
#' # Text objects with different hashes can be merged.
#' # This groups Text objects according to their hashes
#' # and calls c() on each group. It returns a list.
#' merge_texts(txt1, txt2, txt3)
#'
#' # Objects can be coerced to a Text object with as_text(). Below is an
#' # example for call objects. This is for illustration purposes only,
#' # and the latter should not be used. It is worthwhile to note that this
#' # method is used internally by find_source(). Use this function instead.
#' translate_call <- str2lang("transltr::translate('Hello, world!')")
#' translate_loc  <- location("example in class-text", 2L, 32L, 2L, 68L)
#' as_text(translate_call, location = translate_loc)
#'
#' @include constants.R
#'
#' @rdname class-text
#' @keywords internal
#' @export
text <- function(
    ...,
    source_lang = language_source_get(),
    algorithm   = constant("algorithms"))
{
    assert_chr1(source_lang)

    dots  <- list(...)
    texts <- dots[vapply_1l(dots, is.character)]
    locs  <- dots[vapply_1l(dots, is_location)]

    if (!is_match(source_lang, names(texts))) {
        stops(
            "a translation corresponding to 'source_lang' must be passed to '...'.\n",
            "It is treated as the source text.")
    }

    txt <- Text$new(algorithm)
    do.call(txt$set_translations, texts)
    do.call(txt$set_locations, locs)
    txt$source_lang <- source_lang
    return(txt)
}

#' @rdname class-text
#' @keywords internal
#' @export
is_text <- function(x) {
    return(inherits(x, "Text"))
}

#' @rdname class-text
#' @export
format.Text <- function(x, ...) {
    if (length(locations <- lapply(x$locations, format))) {
        names(locations) <- basename(names(locations))
    }

    xlist <- list(
        Hash          = x$hash,
        `Source Lang` = x$source_lang,
        Algorithm     = x$algorithm,
        Translations  = x$translations,
        Locations     = locations)

    return(c("<Text>", format_vector(xlist, level = 1L)))
}

#' @rdname class-text
#' @export
print.Text <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    return(invisible(x))
}

#' @rdname class-text
#' @export
c.Text <- function(...) {
    if (...length() < 2L) {
        return(..1)
    }
    if (!all(vapply_1l(texts <- list(...), is_text))) {
        stops("values passed to '...' must all be 'Text' objects.")
    }

    hashes <- vapply_1c(texts, `[[`, i = "hash")

    # Checking hashes simultaneously checks equality
    # of algorithm, source_lang and source_text.
    if (!all(hashes[[1L]] == hashes[-1L])) {
        stops("all 'hash' must be equal in order to combine 'Text' objects.")
    }
    if (hashes[[1L]] == constant("unset")) {
        stops("all 'Text' objects have no source language set.")
    }

    # Names of inputs are stripped. Otherwise,
    # unlist() alters named character vectors
    # stemming from $languages.
    names(texts) <- NULL

    trans <- unlist(lapply(texts, `[[`, i = "translations"))
    locs  <- unlist(lapply(texts, `[[`, i = "locations"), FALSE)

    txt <- Text$new(..1$algorithm)
    do.call(txt$set_translations, as.list(trans))
    do.call(txt$set_locations, locs)
    txt$source_lang <- ..1$source_lang
    return(txt)
}

#' @rdname class-text
#' @keywords internal
#' @export
merge_texts <- function(..., algorithm = constant("algorithms")) {
    if (!all(vapply_1l(texts <- list(...), is_text))) {
        stops("values passed to '...' must all be 'Text' objects.")
    }

    assert_arg(algorithm, TRUE)
    lapply(texts, \(txt) txt$algorithm <- algorithm)

    # Texts with no hash have no set source text
    # and source language. These Texts cannot be
    # merged and must be ignored.
    hashes <- vapply_1c(texts, `[[`, i = "hash")
    is_set <- hashes != constant("unset")
    groups <- unname(split(texts[is_set], hashes[is_set]))

    return(lapply(groups, \(group) do.call(c, group)))
}

#' @rdname class-text
#' @keywords internal
#' @export
as_text <- function(x, ...) {
    UseMethod("as_text")
}

#' @rdname class-text
#' @export
as_text.call <- function(x,
    strict    = FALSE,
    location  = transltr::location(),
    algorithm = constant("algorithms"),
    validate  = TRUE,
    ...)
{
    assert_lgl1(validate)

    if (validate) {
        assert_lgl1(strict)

        if (!is_translate_call(x, strict)) {
            stops("'x' must be a 'call' object to 'transltr::translate()'.")
        }
        if (!is_location(location)) {
            stops("'location' must be a 'Location' object.")
        }
    }

    # First element of a call is the
    # name of the underlying function.
    args        <- as.list(match.call(translate, x, expand.dots = FALSE))[-1L]
    dots        <- unlist(args$`...`, use.names = FALSE)
    concat      <- args$concat      %??% constant("concat")
    source_lang <- args$source_lang %??% language_source_get()

    txt <- Text$new(algorithm)
    txt$set_locations(location)
    txt$set_translation(source_lang, normalize(dots, concat = concat))
    txt$source_lang <- source_lang
    return(txt)
}

#' @rdname class-text
#' @keywords internal
#' @export
Text <- R6::R6Class("Text",
    lock_class   = TRUE,
    lock_objects = TRUE,
    cloneable    = FALSE,
    private      = list(
        .hash         = constant("unset"),  # See $hash
        .algorithm    = constant("unset"),  # See $algorithm
        .source_lang  = constant("unset"),  # See $source_lang
        .translations = NULL,               # See $translations
        .locations    = NULL                # See $locations
    ),
    active = list(
        #' @field hash A non-empty and non-[NA][base::NA] character string. A
        #'   reproducible hash generated from `source_lang` and `source_text`,
        #'   and by using the algorithm specified by `algorithm`. It is used
        #'   as a unique identifier for the underlying [`Text`][Text] object.
        #'
        #'   This is a **read-only** field. It is automatically updated
        #'   whenever fields `source_lang` and/or `algorithm` are updated.
        hash = \(value) {
            if (!missing(value)) {
                stops(
                    "'hash' cannot be overwritten.\n",
                    "Update it by setting 'source_lang' instead.")
            }

            return(private$.hash)
        },

        #' @template field-algorithm
        algorithm = \(value) {
            if (!missing(value)) {
                assert_algorithm <- \(algorithm = constant("algorithms")) {
                    assert_arg(algorithm, TRUE)
                    return(algorithm)
                }

                private$.algorithm <- assert_algorithm(value)

                if (!is.null(source_text <- self$source_text)) {
                    private$.hash <- hash(
                        self$source_lang,
                        source_text,
                        value)
                }
            }

            return(private$.algorithm)
        },

        #' @template field-source-lang
        source_lang = \(value) {
            if (!missing(value)) {
                assert_chr1(value, x_name = "source_lang")
                assert_match(value, self$languages,
                    quote_values = TRUE,
                    x_name       = "source_lang")

                private$.source_lang <- value
                private$.hash <- hash(
                    value,
                    self$get_translation(value),
                    self$algorithm)
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

            return(self$get_translation(self$source_lang))
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
        #' @description Create a [`Text`][Text] object.
        #'
        #' @template param-algorithm
        #'
        #' @return An [`R6`][R6::R6] object of class [`Text`][Text].
        #'
        #' @examples
        #' # Consider using text() instead.
        #' txt <- Text$new()
        initialize = \(algorithm = constant("algorithms")) {
            private$.translations <- new.env(parent = emptyenv())
            private$.locations    <- new.env(parent = emptyenv())

            self$algorithm <- algorithm
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
        #'
        #' @examples
        #' txt <- Text$new()
        #' txt$set_translation("en", "Hello, world!")
        #'
        #' txt$get_translation("en")  ## Outputs "Hello, world!"
        #' txt$get_translation("fr")  ## Outputs NULL
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
        #' @return A `NULL`, invisibly.
        #'
        #' @examples
        #' # Register a pair of source_lang and source_text.
        #' txt <- Text$new()
        #' txt$set_translation("en", "Hello, world!")
        #' txt$source_lang <- "en"
        set_translation = \(lang = "", text = "") {
            assert_chr1(lang)
            assert_chr1(text, TRUE)
            private$.translations[[lang]] <- text
            return(invisible())
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
        #' @return A `NULL`, invisibly.
        #'
        #' @examples
        #' txt <- Text$new()
        #' txt$set_translations(en = "Hello, world!", fr = "Bonjour, monde!")
        set_translations = \(...) {
            if (!...length()) {
                return(invisible())
            }
            if (!all(vapply_1l(trans <- list(...), is_chr1))) {
                stops("values passed to '...' must all be non-NA and non-empty character strings.")
            }

            assert_named(trans, x_name = "...")
            list2env(trans, private$.translations)
            return(invisible())
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
        #' @return A `NULL`, invisibly.
        #'
        #' @examples
        #' txt <- Text$new()
        #' txt$set_locations(
        #'   location("a", 1L, 2L, 3L, 4L),
        #'   location("a", 1L, 2L, 3L, 4L),
        #'   location("b", 5L, 6L, 7L, 8L))
        set_locations = \(...) {
            if (!...length()) {
                return(invisible())
            }

            locs <- c(as.list(private$.locations), list(...))
            locs <- do.call(merge_locations, locs)
            names(locs) <- vapply_1c(locs, `[[`, i = "path")

            list2env(locs, private$.locations)
            return(invisible())
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
        #' @return A `NULL`, invisibly.
        #'
        #' @examples
        #' txt <- Text$new()
        #' txt$set_translations(en = "Hello, world!", fr = "Bonjour, monde!")
        #' txt$source_lang <- "en"
        #'
        #' # Remove source_lang and source_text.
        #' txt$source_lang <- "fr"
        #' txt$rm_translation("en")
        rm_translation = \(lang = "") {
            assert_chr1(lang)

            if (lang == self$source_lang) {
                stopf(
                    "'%s' is the current 'source_lang'. %s",
                    lang, "Set a new one before removing it.")
            }

            langs <- self$languages
            assert_match(lang, langs[langs != self$source_lang], quote_values = TRUE)

            rm(list = lang, envir = private$.translations)
            return(invisible())
        },

        #' @description Remove a registered location.
        #'
        #' @param path A non-empty and non-[NA][base::NA] character string
        #'   identifying a [`Location`][Location] object to be removed.
        #'
        #' @return A `NULL`, invisibly.
        #'
        #' @examples
        #' txt <- Text$new()
        #' txt$set_locations(
        #'   location("a", 1L, 2L, 3L, 4L),
        #'   location("b", 5L, 6L, 7L, 8L))
        #'
        #' txt$rm_location("a")
        rm_location = \(path = "") {
            assert_chr1(path)
            assert_match(path, names(private$.locations), quote_values = TRUE)

            rm(list = path, envir = private$.locations)
            return(invisible())
        }
    )
)
