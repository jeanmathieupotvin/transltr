#' Read and Write Translations
#'
#' @description
#' Read (import) *Portable Translator/Translations Files*, and convert them
#' to [`Translator`][Translator] objects.
#'
#' Write (export) [`Translator`][Translator] objects as
#' *Portable Translators/Translations Files*.
#'
#' @details
#' [translator_write()] creates two types of file: a single
#' *Portable Translator File*, and further *Portable Translations Files*. Both
#' can be referred to as `PTF` files, and jointly represents all the information
#' contained within a [`Translator`][Translator] object. As such, PTF files are
#' closely tied by design, work together, but target different audiences:
#'
#'   * Portable Translator Files are useful to developers, and
#'   * Portable Translations Files are useful to non-technical collaborators
#'     such as translators.
#'
#' ## Portable Translator File
#'
#' A Portable Translator File (PTF) is a human-friendly, cross language,
#' and textual representation (serialization) of a [`Translator`][Translator]
#' object. The format heavily relies on [YAML 1.1](https://yaml.org/spec/1.1/),
#' a popular data serialization format. However, users do not actually have to
#' know anything about YAML to read, or write [`Translator`][Translator]
#' objects.
#'
#' Portable Translator Files are snapshots of [`Translator`][Translator]
#' objects. To ease collaboration and maintenance, translations are grouped by
#' language, and stored in separate Portable Translations Files). These files
#' are listed and referenced by the `Translations Files` field of a Portable
#' Translator File.
#'
#' ## Portable Translations File
#'
#' A Portable Translations File (PTF) is a human-friendly, cross language,
#' and textual representation (serialization) of the translations contained
#' by a [`Translator`][Translator] object. It has two parts:
#'
#'   1. a YAML header containing basic information on source and target
#'        languages, and
#'   2. a sequence of unindented (flat) and named sections.
#'
#' See [export()] for more information.
#'
#' Note that [translator_write()] never creates a Portable Translations File
#' for the source language itself. Attempting to translate the source text to
#' itself makes no sense.
#'
#' ## Working with PTF files
#'
#' The easiest way to import translations is to use [translator_read()]. This
#' function is designed to read all related PTFs, extract their contents, and
#' construct a suitable [`Translator`][Translator] object from it. This is the
#' preferred interface. The same is true for [translator_write()].
#'
#' Users may read, or write individual Portable Translations Files with
#' [translations_read()], and [translations_write()], respectively. This
#' can be useful for debugging purposes. See Examples below.
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
#' ## Requirements
#'
#' To export a [`Translator`][Translator], the underlying registered
#' [`Text`][Text] objects must have the same source language (the same
#' [`Text$source_lang`][Text] value). An error is thrown otherwise.
#'
#' @param path A character string. A path to a file to read from, or write to.
#'   Its parent directories are automatically created using [dir.create()] if
#'   they do not exist.
#'
#'   By default, [translator_read()], and [translator_write()] respectively
#'   reads from, and writes to a default location given by global \R option
#'   `transltr.default.path`. It points to a standard
#'   **Portable Translator File**. See Details below.
#'
#' @param tr A [`Translator`][Translator] object.
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
#' side-effects:
#'
#'   * it writes a Portable Translator File to `path`, and
#'   * it writes further Portable Translations File(s) in the same directory
#'     as `path`. One file per non-source native language defined in `x` is
#'     created. Therefore, you may start supporting a new language by simply
#'     adding a new entry to [`Translator$native_languages`][Translator].
#'
#' [translations_read()] returns an S3 object of class
#' [`ExportedTranslations`][export()]. Consider using [translator_read()]
#' instead.
#'
#' [translations_write()] returns `NULL`, invisibly. It is used for its
#' side-effect of creating a single Portable Translations File.
#'
#' @seealso
#' [`Translator`][Translator],
#' [export()]
#'
#' @examples
#' # In what follows, ASCII characters are preferred because R has poor
#' # support for non-ASCII characters used in man pages, and they must
#' # be used cautiously. In practice, any alphabet (any UTF-8 character)
#' # may be used to represent native languages and translations.
#'
#' # Set source language.
#' language_source_set("en")
#'
#' # Define a location where Portable Translator Files are written.
#' temp_path <- tempfile(pattern = "_translator_", fileext = ".yml")
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
#' #   1. 1 Portable Translator File for the Translator object, and
#' #   2. 2 Portable Translations Files (one for each non-source language).
#' #      The file for language "es" contains placeholders for future
#' #      translations.
#' translator_write(tr, temp_path)
#' translator_read(temp_path)
#'
#' # Inspect their (raw) contents.
#' cat(readLines(temp_path), sep = "\n")
#' cat(readLines(file.path(dirname(temp_path), "fr.txt")), sep = "\n")
#' cat(readLines(file.path(dirname(temp_path), "es.txt")), sep = "\n")
#'
#' # Translations can be read individually.
#' # They are serialized as a YAML string
#' # (on-the-fly) before being displayed.
#' translations_read(file.path(dirname(temp_path), "fr.txt"))
#' translations_read(file.path(dirname(temp_path), "es.txt"))
#'
#' # This is rarely useful, but translations can also be
#' # exported individually. You may use this to add a new
#' # language, as long as
#' #
#' #   1. it has a corresponding entry in the underlying Translator object, and
#' #   2. it has corresponding entries in the underlying Portable Translator
#' #      File (fields languages and `Translations Files` must be updated).
#' #
#' # Users should always used translator_write() instead.
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
    if (!is_translator(tr <- deserialize(text_read(path, encoding)))) {
        stops("'path' must refer to a text file containing a serialized 'Translator' object.")
    }

    # Read all (Portable) Translations Files,
    # import them, and register translations.
    files <- translations_files(tr)
    path  <- normalizePath(path, mustWork = FALSE)
    lapply(file.path(dirname(path), files), translations_read, encoding, tr)

    return(tr)
}

#' @rdname translator-io
#' @export
translator_write <- function(
    tr   = translator(),
    path = getOption("transltr.default.path"))
{
    if (!is_translator(tr)) {
        stops("'tr' must be a 'Translator' object.")
    }

    files    <- translations_files(tr)
    path_dir <- create_parent_dir(path)
    comments <- c(
        "# Portable Translator File",
        "#",
        "# Instructions:",
        "#  - You may edit Identifier, Languages, and Translations Files.",
        "#  - Do not edit other fields by hand; edit source scripts instead.",
        "#  - Field _Uuid uniquely identifies underlying objects.",
        "#  - Translations are stored in distinct Portable Translations Files.",
        "#  - File paths listed under Translations Files must be relative to this file.",
        "%YAML 1.1",
        "---")

    # Write the Portable Translator File.
    text_write(c(comments, serialize(tr)), path)

    # Write Portable Translations files (one
    # per defined native language). They are
    # written in the same directory as path.
    map(translations_write,
        path = file.path(path_dir, files),
        lang = names(files),
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
        "# Portable Translations File",
        "#",
        "# Instructions:",
        "#  - Edit each 'Translation' section below.",
        "#    - Translate what is in each preceding 'Source Text' section.",
        "#    - Do not edit other sections.",
        "#    - Split long sentences. Single new lines are treated as spaces.",
        "#  - Choose UTF-8 whenever you have to select a character encoding.",
        "#  - You may use any text editor.",
        "#  - You may include comments.",
        "#    - What follows an octothorpe (#) is ignored until the next line.",
        "#    - An escaped octothorpe (\\#) is treated as normal text.",
        "#  - Developers may split long sentences into a multi-line string.",
        "")

    return(text_write(c(comments, serialize_translations(tr, lang)), path))
}
