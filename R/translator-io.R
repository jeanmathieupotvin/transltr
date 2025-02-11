#' Read and Write Translations
#'
#' Export [`Translator`][Translator] objects to text files and import such
#' files back into \R as [`Translator`][Translator] objects.
#'
#' @details
#' The information contained within a [`Translator`][Translator] object is
#' split: translations are reorganized by language and exported independently
#' from other fields.
#'
#' [translator_write()] creates two types of file: a single *Translator file*,
#' and zero, or more *translations files*. These are plain text files that can
#' be inspected and modified using a wide variety of tools and systems. They
#' target different audiences:
#'
#'   * the Translator file is useful to developers, and
#'   * translations files are meant to be shared with non-technical
#'     collaborators such as translators.
#'
#' [translator_read()] first reads a Translator file and creates a
#' [`Translator`][Translator] object from it. It then calls
#' [translations_paths()] to list expected translations files (that should
#' normally be stored alongside the Translator file), attempts to read them,
#' and registers successfully imported translations.
#'
#' There are two requirements.
#'
#'   * All files must be stored in the same directory. By default, this is set
#'     equal to `inst/transltr/` (see `getOption("transltr.path")`).
#'   * Filenames of translations files are standardized and must correspond to
#'     languages (language codes, see `lang`).
#'
#' The inner workings of the serialization process are thoroughly described in
#' [serialize()].
#'
#' ## Translator file
#'
#' A Translator file contains a [YAML](https://yaml.org/spec/1.1/) (1.1)
#' representation of a [`Translator`][Translator] object stripped of all
#' its translations except those that are registered as source text.
#'
#' ## Translations files
#'
#' A translations file contains a [FLAT][flat_serialize()] representation of
#' a set of translations sharing the same target language. This format attempts
#' to be as simple as possible for non-technical collaborators.
#'
#' @param path A non-empty and non-NA character string. A path to a file to
#'   read from, or write to.
#'
#'   * This file must be a Translator file for [translator_read()].
#'   * This file must be a translations file for [translations_read()].
#'
#'   See Details for more information. [translator_write()] automatically
#'   creates the parent directories of `path` (recursively) if they do not
#'   exist.
#'
#' @param tr A [`Translator`][Translator] object.
#'
#'   This argument is `NULL` by default for [translations_read()]. If a
#'   [`Translator`][Translator] object is passed to this function, it
#'   will read translations and further register them (as long as they
#'   correspond to an existing source text).
#'
#' @param overwrite A non-NA logical value. Should existing files be
#'   overwritten? If such files are detected and `overwrite` is set equal
#'   to `TRUE`, an error is thrown.
#'
#' @param translations A non-NA logical value. Should translations files also
#'   be read, or written along with `path` (the Translator file)?
#'
#' @param parent_dir A non-empty and non-NA character string. A path to a
#'   parent directory.
#'
#' @template param-encoding
#'
#' @template param-lang
#'
#' @template param-verbose
#'
#' @returns
#' [translator_read()] returns an [`R6`][R6::R6] object of class
#' [`Translator`][Translator].
#'
#' [translator_write()] returns `NULL`, invisibly. It is used for its
#' side-effects of
#'
#'   * creating a Translator file to the location given by `path`, and
#'   * creating further translations file(s) in the same directory if
#'     `translations` is `TRUE`.
#'
#' [translations_read()] returns an S3 object of class
#' [`ExportedTranslations`][export()].
#'
#' [translations_write()] returns `NULL`, invisibly.
#'
#' [translations_paths()] returns a named character vector.
#'
#' @seealso
#' [`Translator`][Translator],
#' [serialize()]
#'
#' @examples
#' # Set source language.
#' language_source_set("en")
#'
#' # Create a path to a temporary Translator file.
#' temp_path <- tempfile(pattern = "translator_", fileext = ".yml")
#' temp_dir  <- dirname(temp_path)  ## tempdir() could also be used
#'
#' # Create a Translator object.
#' # This would normally be done by find_source(), or translator_read().
#' tr <- translator(
#'   id = "test-translator",
#'   en = "English",
#'   es = "Español",
#'   fr = "Français",
#'   text(
#'     en = "Hello, world!",
#'     fr = "Bonjour, monde!"),
#'   text(
#'     en = "Farewell, world!",
#'     fr = "Au revoir, monde!"))
#'
#' # Export it. This creates 3 files: 1 Translator file, and 2 translations
#' # files because two non-source languages are registered. The file for
#' # language "es" contains placeholders and must be completed.
#' translator_write(tr, temp_path)
#' translator_read(temp_path)
#'
#' # Translations can be read individually.
#' translations_files <- translations_paths(tr, temp_dir)
#' translations_read(translations_files[["es"]])
#' translations_read(translations_files[["fr"]])
#'
#' # This is rarely useful, but translations can also be exported individually.
#' # You may use this to add a new language, as long as it has an entry in the
#' # underlying Translator object (or file).
#' tr$set_native_languages(el = "Greek")
#'
#' translations_files <- translations_paths(tr, temp_dir)
#'
#' translations_write(tr, translations_files[["el"]], "el")
#' translations_read(file.path(temp_dir, "el.txt"))
#'
#' @rdname translator-io
#' @export
translator_read <- function(
    path         = getOption("transltr.path"),
    encoding     = "UTF-8",
    verbose      = getOption("transltr.verbose", TRUE),
    translations = TRUE)
{
    assert_lgl1(verbose)
    assert_lgl1(translations)

    string <- paste0(text_read(path, encoding), collapse = "\n")
    tr     <- deserialize(string)

    # translations_paths() checks that tr has
    # a single source language before reading
    # translations files.
    transl_files <- translations_paths(tr, dirname(path))

    if (translations) {
        lapply(transl_files, \(path) {
            if (verbose) {
                cat(sprintf("Reading translations from '%s'.", path), sep = "\n")
            }

            tryCatch({
                # tr is updated by reference via import().
                lang <- translations_read(path, encoding, tr)[["Language Code"]]
            },
            error = \(err) {
                # Do not throw an error if something goes wrong
                # when verbose is TRUE. Report the error as a
                # console output and move on to the next file.
                if (verbose) {
                    cat("Error(s): ", err$message, "\n", sep = "")
                    return(invisible())
                }

                # NOTE: this line of code is covered by 2 expectations
                # in the test block "translator_read() reports errors",
                # but covr sees it as being uncovered. Disabling its
                # coverage until a fix is found.
                stopf("in '%s': %s", path, err$message) # nocov
            })

            return(invisible())
        })
    }

    return(tr)
}

#' @rdname translator-io
#' @export
translator_write <- function(
    tr           = translator(),
    path         = getOption("transltr.path"),
    overwrite    = FALSE,
    verbose      = getOption("transltr.verbose", TRUE),
    translations = TRUE)
{
    assert_chr1(path)
    assert_lgl1(overwrite)
    assert_lgl1(verbose)
    assert_lgl1(translations)

    if (!overwrite && file.exists(path)) {
        stops("'path' already exists. Set 'overwrite' equal to 'TRUE'.")
    }
    if (!dir.exists(parent_dir <- dirname(path)) &&
        !dir.create(parent_dir, FALSE, TRUE)) {
        stops("parent directory of 'path' could not be created.")
    }

    # translations_paths() checks that tr is a
    # Translator and has a single source language.
    transl_paths <- translations_paths(tr, parent_dir)

    if (translations) {
        # Write Exported Translations (one per non-source
        # native language) in the same directory as path.
        map(path = transl_paths, lang = names(transl_paths), \(path, lang) {
            if (verbose) {
                cat(sprintf("Writing '%s' translations to '%s'.", lang, path),
                    sep = "\n")
            }

            translations_write(tr, path, lang)
        })
    }

    comments <- c(
        "# Translator",
        "#",
        "# - You may edit fields Identifier and Languages.",
        "# - Do not edit other fields by hand. Edit source scripts instead.",
        "%YAML 1.1",
        "---")

    text_write(c(comments, serialize(tr)), path)
    return(invisible())
}

#' @rdname translator-io
#' @export
translations_read <- function(path = "", encoding = "UTF-8", tr = NULL) {
    string <- paste0(text_read(path, encoding), collapse = "\n")
    return(deserialize_translations(string, tr))
}

#' @rdname translator-io
#' @export
translations_write <- function(tr = translator(), path = "", lang = "") {
    comments <- c(
        "# Translations",
        "#",
        "# - Edit each 'Translation' subsection below.",
        "# - Do not edit 'Source Text' subsections.",
        "# - Choose UTF-8 whenever you have to select a character encoding.",
        "# - You may use any text editor.",
        "# - You may split long sentences with single new lines.",
        "# - You may separate paragraphs by leaving a blank line between them.",
        "# - You may include comments.",
        "#   - What follows an octothorpe (#) is ignored until the next line.",
        "#   - An escaped octothorpe (\\#) is treated as normal text.",
        "")

    text_write(c(comments, serialize_translations(tr, lang)), path)
    return(invisible())
}

#' @rdname translator-io
#' @export
translations_paths <- function(
    tr         = translator(),
    parent_dir = dirname(getOption("transltr.path")))
{
    assert_chr1(parent_dir)

    if (!is_translator(tr)) {
        stops("'tr' must be a 'Translator' object.")
    }
    if (length(source_lang <- tr$source_langs) > 1L) {
        stops("all 'Text' objects of 'tr' must have the same 'source_lang'.")
    }

    native_langs <- tr$native_languages
    native_langs <- native_langs[names(native_langs) != tr$source_langs]

    langs <- names(native_langs)
    files <- file.path(parent_dir, sprintf("%s.txt", langs))
    names(files) <- langs

    return(files)
}
