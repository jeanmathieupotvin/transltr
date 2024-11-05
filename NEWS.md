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

# `transltr` 0.0.1.9000

Initial development version.
