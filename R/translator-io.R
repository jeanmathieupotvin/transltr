#' Read and Write Translations
#'
#' Export [`Translator`][Translator] objects to text files, and import such
#' files back into \R as [`Translator`][Translator] objects.
#'
#' @details
#' To ease collaboration and maintenance, the information contained within
#' a [`Translator`][Translator] object is split. Translations are exported
#' independently from other fields. Two types of objects are created from
#' a [`Translator`][Translator] object: a single *Exported Translator*, and
#' zero, or more *Exported Translations*. These objects are portable because
#' they are textual representations that can be inspected, and modified using
#' a wide variety of tools and systems.
#'
#' Exported objects are stored in plain text files. The inner workings of the
#' serialization process are thoroughly described in [serialize()].
#'
#' [translator_write()] creates two types of file: one file containing an
#' Exported Translator, and further file(s) containing Exported Translations.
#' They target different audiences:
#'
#'   * Exported Translator files are useful to **developers**, and
#'   * Exported Translations files are useful to **non-technical collaborators**
#'     such as translators.
#'
#' ## Exported Translator
#'
#' An Exported Translator is a [YAML 1.1](https://yaml.org/spec/1.1/)
#' representation of a [`Translator`][Translator] object stripped of all
#' its translations except those that are registered as source text. It
#' references related Exported Translations files via a `Translations Files`
#' field.
#'
#' ## Exported Translations
#'
#' Translations registered in a [`Translator`][Translator] object are extracted,
#' grouped by language, and stored in distinct file(s). One Exported
#' Translations is created for each non-source native language registered in
#' `x`. Fore more information, see [`Translator$native_languages`][Translator].
#'
#' An Exported Translations is a [FLAT 1.0][flat_serialize()] representation.
#' This format is custom, and attempts to be as simple as possible for
#' non-technical collaborators.
#'
#' @param path A non-empty and non-[NA][base::NA] character string. A path to
#'   a file to read from, or write to.
#'
#'   * This file must be an Exported Translator file for [translator_read()].
#'   * This file must be an Exported Translations file for [translations_read()].
#'
#'   See Details for more information. [translator_write()] automatically
#'   creates the parent directory of `path` (recursively) if it does not exist.
#'
#' @param tr A [`Translator`][Translator] object.
#'
#' @param overwrite A non-[NA][base::NA] logical value. Should an existing
#'   *Exported Translator* file (pointed to by `path`) be overwritten? An
#'   error is thrown otherwise.
#'
#' @template param-encoding
#'
#' @template param-lang
#'
#' @returns
#' [translator_read()] returns an [`R6`][R6::R6] object of class
#' [`Translator`][Translator].
#'
#' [translator_write()] returns `NULL`, invisibly. It is used for its
#' side-effects of simultaneously
#'
#'   * creating an Exported Translator file to the location given by `path`, and
#'   * creating further Exported Translations file(s) in the same directory as
#'     the Exported Translator file (by default).
#'
#' Note that underlying entries of `Translations Files` (a standard field of
#' Exported Translator objects) may be modified afterwards. The file paths it
#' contains are cached by [translations_read()], and reused automatically.
#'
#' [translations_read()] returns an S3 object of class
#' [`ExportedTranslations`][export()].
#'
#' [translations_write()] returns `NULL`, invisibly. It is used for its
#' side-effect of creating a single Exported Translations file to the location
#' given by `path`.
#'
#' @seealso
#' [`Translator`][Translator],
#' [serialize()]
#'
#' @examples
#' # In what follows, ASCII characters are preferred because R has poor
#' # support for non-ASCII characters used in documentation (help pages),
#' # and they must be used cautiously. In practice, any alphabet (any UTF-8
#' # character) may be used to represent native languages and translations.
#'
#' # Set source language.
#' language_source_set("en")
#'
#' # Create a path to a (future) Exported Translator file.
#' temp_path <- tempfile(pattern = "translator_", fileext = ".yml")
#'
#' # Create a Translator object.
#' # This would normally be done automatically
#' # by find_source(), or translator_read().
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
#' # Export it.
#' # This creates 3 files:
#' #
#' #   1. 1 Exported Translator file, and
#' #   2. 2 Exported Translations files (one for language "es", and another
#' #      one for language "fr"). The file for language "es" contains
#' #      placeholders for future translations.
#' translator_write(tr, temp_path)
#' translator_read(temp_path)
#'
#' # Inspect their (raw) contents.
#' cat(readLines(temp_path), sep = "\n")
#' cat(readLines(file.path(dirname(temp_path), "fr.txt")), sep = "\n")
#' cat(readLines(file.path(dirname(temp_path), "es.txt")), sep = "\n")
#'
#' # Translations can be read individually.
#' translations_read(file.path(dirname(temp_path), "fr.txt"))
#' translations_read(file.path(dirname(temp_path), "es.txt"))
#'
#' # This is rarely useful, but translations can also be
#' # exported individually. You may use this to add a new
#' # language, as long as
#' #
#' #   1. it has a corresponding entry in the underlying Translator object, and
#' #   2. it has corresponding entries in the underlying Exported Translator
#' #      (fields 'Languages' and 'Translations Files' must be updated).
#' #
#' # Consider using translator_write() instead.
#' tr$set_native_languages(el = "Greek")
#' translations_write(tr, file.path(dirname(temp_path), "el.txt"), "el")
#' translations_read(file.path(dirname(temp_path), "el.txt"))
#'
#' @rdname translator-io
#' @export
translator_read <- function(
    path     = getOption("transltr.default.path"),
    encoding = "UTF-8")
{
    string <- paste0(text_read(path, encoding), collapse = "\n")
    tr     <- deserialize(string)

    # Read all Exported Translations files,
    # import them, and register translations.
    lapply(translations_files(tr), translations_read, encoding, tr)

    return(tr)
}

#' @rdname translator-io
#' @export
translator_write <- function(
    tr        = translator(),
    path      = getOption("transltr.default.path"),
    overwrite = FALSE)
{
    # tr object is validated below by call to
    # translations_files(). It requires a valid
    # path, so it is thoroughly checked before.
    assert_chr1(path)
    assert_lgl1(overwrite)

    path_abs     <- normalizePath(path, mustWork = FALSE)
    path_dir     <- dirname(path)
    path_dir_abs <- dirname(path_abs)

    if (!overwrite && file.exists(path_abs)) {
        # Path is shown instead of its
        # absolute/canonical equivalent
        # to avoid confusion.
        stopf(
            "'path' ('%s') already exists. Set 'overwrite' equal to 'TRUE' before proceeding.",
            path)
    }
    if (!dir.exists(path_dir_abs) &&
        !dir.create(path_dir_abs, TRUE, TRUE) || .__LGL_DEBUG_FLAG) {
        # Absolute path to directory is shown,
        # because it likely will be required
        # for debugging purposes.
        stopf(
            "parent directory of path ('%s') could not be created. %s",
            "Create it manually, or change 'path'.",
            path_dir_abs)
    }

    transl_paths <- translations_files(tr, path_dir)
    comments     <- c(
        "# Exported Translator",
        "#",
        "# Instructions:",
        "#  - You may edit Identifier, Languages, and Translations Files.",
        "#  - Do not edit other fields by hand; edit source scripts instead.",
        "#  - Field _Uuid uniquely identifies underlying objects.",
        "#  - Translations are stored in distinct Exported Translations files.",
        "%YAML 1.1",
        "---")

    # Write the Exported Translator.
    text_write(c(comments, serialize(tr, parent_dir = path_dir)), path)

    # Write Exported Translations (one per
    # non-source native language). They are
    # written in the same directory as path.
    map(translations_write,
        path = transl_paths,
        lang = names(transl_paths),
        more = list(tr = tr))

    return(invisible())
}

#' @rdname translator-io
#' @export
translations_read <- function(path = "", encoding = "UTF-8", tr) {
    string <- paste0(text_read(path, encoding), collapse = "\n")
    return(deserialize_translations(string, tr))
}

#' @rdname translator-io
#' @export
translations_write <- function(tr = translator(), path = "", lang = "") {
    comments <- c(
        "# Exported Translations",
        "#",
        "# Instructions:",
        "#  - Edit each 'Translation' section below.",
        "#    - Translate what is in each preceding 'Source Text' section.",
        "#    - Do not edit other sections.",
        "#  - Choose UTF-8 whenever you have to select a character encoding.",
        "#  - You may use any text editor.",
        "#  - You may split long sentences with single new lines.",
        "#  - You may include comments.",
        "#    - What follows an octothorpe (#) is ignored until the next line.",
        "#    - An escaped octothorpe (\\#) is treated as normal text.",
        "")

    text_write(c(comments, serialize_translations(tr, lang)), path)
    return(invisible())
}
