# `transltr` 0.0.1.9004

This version is the penultimate development version prior to an initial
release of version 0.0.1 (and submission to CRAN). It brings major changes
to the Portable Translator Format/File (PTF), previously known as TSF.

## New features

1. New features to import, and export translations (it replaces the older
   Markdown file and related parsing mechanisms).

   * Translations Source Files (TSF) are replaced by
     Portable Translator/Translations File (PTF).
   * PTF are generated from *Portable Objects*, which are intermediate classes
     (between textual representations) and `R6` classes `Location`, `Text`,
     and `Translator`. Their role is to ease bidirectional conversion process.
   * `transltr` splits the state of a `Translator` object into multiple files:
     one file for the `Translator` object itself, and further files for
     translations (one per language).
   * This new design introduces many new functions:

     * `text_write()`,
     * `portable()`,
     * `is_portable()`,
     * `portable_translator()`,
     * `portable_text()`,
     * `portable_location()`,
     * `portable_translations()`,
     * `format.Portable()`,
     * `format.PortableTranslator()`,
     * `format.PortableTranslations()`,
     * `print.Portable()`,
     * `as_translator()`,
     * `as_translator.PortableTranslator()`,
     * `as_text.PortableText()`,
     * `as_text.Text()`,
     * `as_location.PortableLocation()`,
     * `as_location.Location()`,
     * `translator_read()`,
     * `translator_write()`,
     * `translations_read()`, and
     * `translations_write()`.

2. New functions `language_source_get()`, and `language_source_set()`. They
   are identical to `language_get()`, and `language_set()`, but sets source
   language instead.

3. New internal interface for constants. They are now encapsulated by a new
   `constant()` function (which is just a `switch()` statement). Fetching
   constants requires a name, i.e. `constant("placeholder")`. This is much
   cleaner.

4. New option `transltr.default.path`. This is where `transltr` reads, and
   writes Portable Translator Files by default. See `translator_read()`, and
   `translator_write()`.

5. New active binding `Translator$source_langs`. It returns `NULL` for empty
   `Translator` objects, a named character vector for `Translator` objects
   having `Text` objects with different `source_lang`, and a character string
   otherwise.

## Changes

1. Class `Block` was renamed to `Text` for better semantics. It is referred to
   as such below and above.

2. Functions `write_text()` and `read_text()` are now respectively named
   `text_write()` and `text_read()` for consistency with other existing
   `text_*()` functions.

3. Functions `write_translations()` and `read_translations()` are now
   respectively named `translator_export()` and `translator_import()` for
   consistency with other existing `translator_*()` functions.

4. Old design for portable translations is deprecated, including all related
   mechanisms. It is replaced by a new design of Portable Objects (that are
   distinct from `.po` files of `gettext()`). See New Features above.

5. Class `Token`, and related features are deprecated. Transitioning to YAML
   removed the need to tokenize the contents of Markdown files. See #3 for
   more information. YAML tags are a much better solution to identify data
   structures.

6. Revamp `format.Location()`. Argument `how` was repurposed and now controls
   how to format ranges. There are three formats available. See documentation.

7. Argument `source_lang` of `text()` now comes after `...`.

8. Function `get_hash_algorithms()` was renamed to `hash_algorithms()`. Other
   `get_*()` functions that used to rerturn "parameters* of the package are all
   deprecated, and were removed. They were either useless with newer designs,
   or replaced by the new internal `constant()` interface. See feature #3 above.

9. `language_set()` has a new error message when it fails to reset current
   language.

10. `Translator$hashes` now returns a named character vector, where names are
   reduced versions of the corresponding hashes.

11. `translator()` now throws a warning if a language is missing an expected
   corresponding native language.

12. `Text` now calls `self$*` more frequently internally (whenever appropriate)
    instead of `private$*`.

13. Documentation, and code comments were updated almost everywhere.

## Issues, and Fixes

1. `Text$hash_algorithm` now only updates `Text$hash` if `Text$source_text`
   (and `Text$source_lang`) is set.

2. `c.Text()` now throws an error if all `Text` objects are empty. A `Text`
   object is empty if it has no set `source_lang`.

3. `merge_texts()` now silently ignores, and drops empty `Text` objects.


---

# `transltr` 0.0.1.9003

This version introduces yet another set of core mechanisms such as the
`Translator` class. See their documentation for more information. We are
near an initial publication to CRAN.

## New features

1. New class `Translator` and related mechanisms.

   * This includes the constructor `translator()`.
   * This includes the introspector `is_block()`.
   * This includes S3 methods `format()`, and `print()`.

2. New functions `translator_set()`, and `translator_get()`.

3. New functions `language_set()`, and `language_get()`.

4. New functions `translate()`, and `is_translate_call()`.

   * It replaces an earlier version.
   * Generic function `match_translate_call()` (and its methods) was deprecated.

5. New functions `uuid()`, `uuid_raw()`, and `uuid_is()`.

6. New functions `text_normalize()`, and `text_hash()`.

7. New set of global constants were introduced.

8. New internal function `format_vector()`.

9. New internal utility function `map()`. It wraps `.mapply()`.

## Changes

1. All `key` arguments were renamed to `lang`. This includes `source_key`.

2. All `find_*()` functions were renamed for consistency.

3. Changes to the `Block` class.

   * Private field `Block$.locations` is now an environment, where names are
     extracted from field `Location$path`.
   * Deprecate intrernal constructor `.block()` in favor of just using the
     class' methods.
   * Some error messages were simplified.
   * `$set_*()` methods now return `NULL` for consistency with other `set_*()`
     functions.
   * Update the documentation.
   * `as_block()` was revamped.

4. Changes to the `Location` class.

   * `format()` was simplified.
   * Documentation was updated.

5. More functions are now exported, but most of them are marked as being
   internal.

4. Some utility functions were renamed.

## Fixes

1. `assert_arg()` now always quote character values.

2. Various typos in the code and in the documentation were fixed.


---

# `transltr` 0.0.1.9002

This version introduces many important core mechanisms such as the `Block`
class and features to extract source texts (that requires) translations from
scripts of a project.

## New features

1. Add support for code profiling. See helper functions `.pf()` and `.sp()` in
`.Rprofile`.

2. Major revamp of class `Location`. It is now much more efficient.

3. Total redesign of class `Block`. It is now an `R6` class in charge of doing
many things, such as hashing source text (among other things). It exposes an
API to safely manipulate locations and translations.

4. New internal functions to manipulate strings: `str_left_pad()`,
`str_trim()`, and `str_sanitize()`. The latter requires more work.

5. Function `find_translations()` and many internal mechanisms to exact
source texts from R scripts. It will be renamed in a future commit.

6. New hashing algorithms (embedded in class `Block`).

7. A lot of new documentation. Still a work in progress for some features.

8. Hundreds of new unit tests.


---

# `transltr` 0.0.1.9001

This is the very first official development version of the package serving as
an inception point. As such, not all *new* features are listed below.

## New features

1. Many lower-level helper functions such as the `is_*()` and `assert_*()`
functions. They are not relevent for users but are super useful to developers.

2. A first Markdown template for so-called Translations Source Files (TSF).

3. A mechanism to efficiently convert TSF back to `R` objects.

4. Low-level S3 classes `Block`, `Location`, and `Token`.

5. Various placeholders for the future.

6. A lot of useful side scripts for developers.


---

# `transltr` 0.0.1.9000

Initial development version.