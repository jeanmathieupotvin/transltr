#' Get or Set Translator Objects
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' **These features are experimental. Use with caution.**
#'
#' Get or set the [`Translator`][Translator] object to use when calling
#' [translate()]. It is registered within an internal cache managed by
#' [`transltr`][transltr].
#'
#' @details
#' Translations performed by [translate()] always have a `scope` specifying
#' which [`Translator`][Translator] object must be used. This scope can be
#' explicit, or implicit.
#'
#' ## Explicit Scopes
#'
#' Explicit scopes are entirely determined, and managed by the user. In that
#' case, argument `scope` of [translator_set()], [translator_get()], and
#' [translate()] must be set to the appropriate value. **Scopes are shared**,
#' and **can be overwritten**. When choosing to use explicit scopes, it is the
#' responsability of the user to ensure its scope(s) are collision-resistant.
#' It may use [translator_scopes()] to get a list of active scopes.
#'
#' ## Implicit Scopes
#'
#' Implicit scopes are the default. These are scopes entirely determined, and
#' managed by [`transltr`][transltr] (unless a user *unsets* them). They are
#' inferred from the underlying [namespace][getNamespace()] of the function
#' that (directly) calls either [translator_set()], [translator_get()], or
#' [translate()]. If it has no namespace, the `global` scope is used. There is
#' one exception: package `base` is always ignored and replaced by the global
#' scope. In other words, the scope will be `global`, unless [translator_set()],
#' [translator_get()], or [translate()] is directly called within a function
#' defined in any non-`base` package.
#'
#' Implicit scopes are determined at runtime by inspecting the call stack. \R
#' call stacks can be surprisingly complex, and in some non-standard situations,
#' [`transltr`][transltr] may fail to infer a scope. Future iterations will
#' bring more robust scoping mechanisms.
#'
#' @section Reference semantics:
#' [`Translator`][Translator] objects are [`R6`][R6::R6Class()] objects, and
#' [`R6`][R6::R6Class()] are stored as [environments][environment()]. In \R,
#' environments have reference semantics (they are not copied when changed).
#' Therefore, a [`Translator`][Translator] object only need to be set once,
#' and may be modified afterwards without having to call [translator_set()]
#' again.
#'
#' Moreover, removing a binding that references a [`Translator`][Translator]
#' object does not remove it from the internal cache. Users further have to
#' call [translator_set()] to do so.
#'
#' @param x A [`Translator`][Translator] object, or `NULL`. The latter is
#'   used to unset a previously registered [`Translator`][Translator] object.
#'
#' @template param-scope
#'
#' @returns
#' [translator_set()] returns `NULL`, invisibly. It is used for its side-effect
#' of registering `x`, and assigning it to a `scope`.
#'
#' [translator_get()] returns the [`Translator`][Translator] object assigned
#' to `scope`, or `NULL` if there is none.
#'
#' [translator_scopes()] returns a character vector.
#'
#' @examples
#' ## Setting and getting a Translator object having a 'global' scope.
#' translator_set(x = translator(id = "my-global-translator"))
#' translator_get()
#' translator_get("global")
#'
#' ## Unregister a Translator object (for implicit and explicit scopes).
#' translator_set(NULL)
#' translator_set(NULL, "global")
#' is.null(translator_get(NULL)) ## TRUE
#'
#' ## Setting and getting a Translator object, while letting them infer
#' ## the underlying scope. This is done temporarily within the utils
#' ## package for illustration purposes.
#' evalq(envir = asNamespace("utils"), \() {
#'   on.exit(translator_set(NULL))
#'   translator_set(translator(id = sprintf("utils:%s", transltr::uuid())))
#'   return(translator_get())
#' })()
#'
#' @seealso [translate()]
#'
#' @rdname translator-accessors
#' @export
translator_set <- function(x = translator(), scope = NULL) {
    scope <- scope %??% translator_scope()
    assert_chr1(scope)

    if (is.null(x)) {
        suppressWarnings(rm(list = scope, pos = .__translators_cache__))
        return(invisible())
    }
    if (!is_translator(x)) {
        stops("'x' must be a 'Translator' object.")
    }

    assign(scope, x, .__translators_cache__)
    return(invisible())
}

#' @rdname translator-accessors
#' @export
translator_get <- function(scope = NULL) {
    scope <- scope %??% translator_scope()
    assert_chr1(scope)
    return(.__translators_cache__[[scope]])
}

#' @rdname translator-accessors
#' @export
translator_scopes <- function() {
    return(sort(names(.__translators_cache__)))
}


# Scopes -----------------------------------------------------------------------


#' Determine Scopes
#'
#' These functions work together to determine the underlying scope of
#' a call to [translator_set()], [translator_get()], or [translate()].
#'
#' [translator_scope_name()] constructs scope names from \R objects.
#'
#'   * It returns `x` as is if it is a character string.
#'   * It returns the [name][environmentName()] of `x` if it is an environment.
#'   * It returns the [name][environmentName()] of the enclosure of `x` if it
#'     is a [function].
#'   * It returns `"global"` for any other value passed to `x`.
#'
#' Empty outputs are always replaced by the default scope (`global`).
#'
#' [translator_scope()] is used to infer scopes by inspecting the call stack.
#' It is called by [translator_set()], [translator_get()], and [translate()].
#' \R call stacks can be surprisingly complex, and in some non-standard
#' situations, [`transltr`][transltr] may fail to infer a scope.
#'
#' @note
#' Developers should absolutely read code comments of these functions before
#' iterating on them.
#'
#' @param x An \R object.
#'
#' @returns A character string.
#'
#' @seealso [sys.nframe()],
#'   [sys.function()],
#'   [translator_set()],
#'   [translator_get()],
#'   [translate()]
#'
#' @rdname translator-scope
#' @keywords internal
translator_scope <- function() {
    # Traverse the call stack and get the scope of each
    # called function. We use sys.function(i) starting
    # at i = -3. This is because the traversal alters
    # the call stack's current state by adding further
    # calls to it. Specifically, 3 calls are added when
    # calling sys.function() in vapply_1c() below. Also,
    # index of sys.function() starts at 0, and calling
    # sys.function(-nframe) always returns NULL, which
    # is used to always add 'global' scope to the vector
    # of candidates.
    nframe <- sys.nframe()
    scopes <- vapply_1c(-seq.int(3L, nframe + 3L, 1L), \(i) {
        return(translator_scope_name(sys.function(i)))
    })

    # Some namespaces are excluded because they are
    # not relevant. Currently, we exclude transltr
    # and base. The former does not concern users,
    # and the latter will never use transltr. This
    # allows embedding translate() calls in other
    # usual base functions. In the future, transltr
    # may still use this function as is to translate
    # itself, because which.max() will return 1 (and
    # match scope "transltr" in scopes).
    excluded_ns <- c("base", "transltr")
    return(scopes[[which.max(!match(scopes, excluded_ns, 0L))]])
}

#' @rdname translator-scope
#' @keywords internal
translator_scope_name <- function(x) {
    scope <- switch(class(x),
        character  = x,
        `function` = {
            name <- environmentName(environment(x))
            if (nzchar(name)) name else "global"
        },
        environment = {
            name <- environmentName(x)
            if (nzchar(name)) name else "global"
        },
        "global")

    if (scope == "R_GlobalEnv") {
        return("global")
    }

    return(if (nzchar(scope)) scope else "global")
}


# I/O --------------------------------------------------------------------------


#' Read and Write Translations
#'
#' @description
#' Read (import) *Portable Translator Files*, and convert them to
#' [`Translator`][Translator] objects.
#'
#' Write (export) [`Translator`][Translator] objects as
#' *Portable Translators Files*.
#'
#' @details
#' A Portable Translator File (PTF) is a human-friendly, cross language,
#' and textual representation (serialization) of a [`Translator`][Translator]
#' object and its underlying translations. The format heavily relies on
#' [YAML 1.1](https://yaml.org/spec/1.1/), a popular data serialization
#' format. However, users do not actually have to know anything about YAML
#' to read, or write [`Translator`][Translator] objects.
#'
#' Portable Translator Files are snapshots of [`Translator`][Translator]
#' objects. To ease collaboration and maintenance, translations are grouped
#' by language, and stored in separate files. These files are always listed
#' by the `translations_files` field of a PTF.
#'
#' As such, a PTF may refer to a *Portable Translator File*, a
#' *Portable Translations File*, or both. Portable Translators and Portable
#' Translations are closely tied by design. It is worthwhile to know that they
#' both have well-defined internal representations referred to as
#' [Portable Objects][portable()].
#'
#' The easiest way to import translations is to use [translator_read()]. This
#' function is designed to read all related PTFs, extract their contents, and
#' construct a suitable [`Translator`][Translator] object from it. This is the
#' preferred interface. The same is true for [translator_write()].
#'
#' Users may read, or write individual Portable Translations Files with
#' [translations_read()], and [translations_write()], respectively. This
#' can be useful for debugging purposes.
#'
#' ## Encodings
#'
#' **The preferred encoding is (and should always be) UTF-8.** If another
#' encoding must be used for some obscure reason, the `encoding` argument
#' has to be updated. Encodings should be known, never inferred.
#'
#' [translator_write()], and [translations_write()] enforces UTF-8. This
#' cannot be changed.
#'
#' @param path A character string. A path to a file to read from, or write to.
#'   Its parent directories are automatically created using [dir.create()] if
#'   they do not exist.
#'
#'   By default, [translator_read()], and [translator_write()] respectively
#'   reads from, and writes to a default location given by global \R
#'   [option][options] `transltr.default.path`. It points to a standard
#'   **Portable Translator File**. See Details below.
#'
#' @param x An object of class [`Translator`][Translator].
#'
#' @template param-encoding
#'
#' @template param-lang
#'
#' @returns
#' [translator_read()] returns a [`Translator`][Translator] object. This is
#' an [R6::R6Class()] instance under the hood.
#'
#' [translator_write()] returns `NULL`, invisibly. It is used for its
#' side-effect of creating Portable Translator Files, and Portable Translations
#' Files.
#'
#' [translations_read()] returns an S3 object of class
#' [`PortableTranslations`][portable()]. Consider using [translator_read()]
#' instead.
#'
#' [translations_write()] returns `NULL`, invisibly. It is used for its
#' side-effect of creating a single Portable Translations File.
#'
#' @seealso
#' [`Translator`][Translator],
#' [`Portable`][portable()],
#' [UTF-8](https://en.wikipedia.org/wiki/UTF-8)
#'
#' @rdname translator-io
#' @export
translator_read <- function(
    path     = getOption("transltr.default.path"),
    encoding = "UTF-8")
{
    text  <- text_read(path, encoding)
    trans <- tryCatch({
        yaml::yaml.load(text,
            # eval.expr is disallowed for security.
            eval.expr     = FALSE,
            as.named.list = TRUE,
            merge.warning = TRUE,
            handlers      = list(
                Translator = as_translator.PortableTranslator,
                Text       = as_text.PortableText,
                Location   = as_location.PortableLocation))
    },
    error = \(cond) {
        stopf(
            "while reading 'Translator' object in '%s': %s",
            path,
            # Below we attempt to formulate a coherent
            # sentence from error messages returned by
            # the YAML parser. This is not perfect, but
            # better than nothing.
            gsub("[ \n\t.]+$", ".", tolower(cond$message)))
    })

    files        <- file.path(dirname(path), attr(trans, "translations_files"))
    translations <- lapply(files, translations_read, encoding = encoding)

    # Loop over each PortableTranslations, and
    # for each of them, loop over all non-NULL
    # translations, and register them.
    lapply(translations, \(tr) {
        lang  <- tr$language
        texts <- lapply(tr$translations, `[[`, i = "translation")
        texts <- texts[!vapply_1l(texts, is.null)]
        map(\(hash, text) trans$get_text(hash)$set_translation(lang, text),
            hash = names(texts),
            text = texts)
    })

    return(trans)
}

#' @rdname translator-io
#' @export
translator_write <- function(
    x    = translator(),
    path = getOption("transltr.default.path"))
{
    assert_chr1(path)
    path       <- normalizePath(path, mustWork = FALSE)
    parent_dir <- dirname(path)

    if (!dir.exists(parent_dir) &&
        !dir.create(dirname(parent_dir), TRUE, TRUE) || .__LGL_DEBUG_FLAG) {
        stopf(
            "parent directory of path ('%s') could not be created. %s",
            "Create it manually, or change 'path'.")
    }

    trans <- portable_translator(x)
    text_write(format(trans, set_instructions = TRUE), path)

    # Write PortableTranslations files (one
    # per defined native language). They are
    # written in the same directory as path.
    paths <- file.path(dirname(path), trans$translations_files)
    langs <- names(trans$translations_files)
    map(translations_write, path = paths, lang = langs, moreArgs = list(x = x))

    return(invisible())
}

#' @rdname translator-io
#' @export
translations_read <- function(path = "", encoding = "UTF-8") {
    # NOTE: this function complies to the flat format. It
    # is tightly coupled with format.PortableTranslations().

    text       <- text_read(path, encoding)
    is_title   <- grepl("^\\[\\[[a-zA-Z0-9]+\\]\\]$", text)
    sec_starts <- which(is_title)
    sec_groups <- cumsum(is_title)
    sec_titles <- gsub("\\[|\\]", "", text[sec_starts])

    # Text is broken into a stream of sections
    # that can be a YAML header, a source text,
    # or a translation. Titles are stripped.
    text <- split_ul(text[-sec_starts], sec_groups[-sec_starts])

    # The first section always corresponds to the
    # raw YAML header. Other sections are split to
    # create pairs of source text and translation.
    header   <- text[[1L]]
    sections <- split_ul(text[-1L], cumsum(sec_titles != "Translation"))
    names(sections) <- sec_titles[sec_titles != "Translation"]

    header <- tryCatch({
        yaml::yaml.load(header,
            # eval.expr is disallowed for security.
            eval.expr     = FALSE,
            as.named.list = TRUE,
            merge.warning = TRUE)
    },
    error = \(cond) {
        stopf(
            "while reading YAML header of translations in '%s': %s",
            path,
            # Below we attempt to formulate a coherent
            # sentence from error messages returned by
            # the YAML parser. This is not perfect, but
            # better than nothing.
            gsub("[ \n\t.]+$", ".", tolower(cond$message)))
    })

    out <- list(
        language               = header$language,
        native_language        = header$native_language,
        source_language        = header$source_language,
        source_native_language = header$source_native_language,
        translations           = lapply(sections, \(section) {
            # There should always be at least one
            # element that corresponds to source
            # text by design.
            source <- section[[1L]]
            source <- if (!is.null(source)) text_normalize(source)

            # Translation may be missing in the
            # case of an invalid file format. `[`
            # returns list(NULL) if this element
            # does not exist.
            trans <- section[2L][[1L]]
            trans <- if (!is.null(trans)) text_normalize(trans)

            return(
                list(
                    source_text = source,
                    translation = if (trans != constant("placeholder")) trans))
        }))

    return(portable(out, "PortableTranslations", "Translations"))
}

#' @rdname translator-io
#' @export
translations_write <- function(x = translator(), path = "", lang = "") {
    assert_chr1(path)
    assert_chr1(lang) # Ensure NULL is always disallowed.

    path    <- normalizePath(path, mustWork = FALSE)
    strings <- format(
        # portable_translations() always returns a list.
        portable_translations(x, lang)[[1L]],
        how = "flat",
        set_instructions = TRUE)

    return(text_write(strings, path))
}


# Internals --------------------------------------------------------------------


# This is an internal cache into which Translator objects
# are registered via translator_set(). Translator objects
# are R6 instances, and these are just fancy environments.
# Environments have reference semantics in R. Therefore,
# even if a Translator has multiple bindings (one for the
# user and one in the cache, typically), changes can be
# made without re-setting objects over and over again.
.__translators_cache__ <- new.env(parent = emptyenv())
