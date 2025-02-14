# `transltr` 0.1.0

## Breaking Changes

* The package's lifecycle is set back to `experimental`.
  * All features are still well tested and documented, but they may
    change again in a near future. I am thinking about introducing many
    small changes to the interface of classes `Text` and `Translator`.
* R `>= 4.3.0` is now required. This is necessary to
  * leverage native `UTF-8` support on Windows,
  * use function shorthands (`\(...)`) and
  * use the native pipe operator `|>` and related placeholders `_`.
* Package `stringi` is now required at runtime.
  * This package dramatically increases the performance of `normalize()`.
  * Its usage within the package will gradually increase over time.
* Option `transltr.default.path` is renamed to `transltr.path`.
* `constant()` is removed. While it technically was an unexported function,
  it was used to pass default values to many function arguments.
* `find_source()` loses argument `strict`. It is replaced by new argument
  `interface`.
* `as_text.call()` loses arguments `strict` and `validate` following the
  new `interface` mechanism of `find_source()`. It was rewritten accordingly.
* `translate()` is removed and replaced by the new `interface` mechanism of
  `find_source()`. See the documentation of the latter for more information.

## New Features

* `find_source()` now detects calls to method `Translator$translate()` by
  default. Users may change this default behavior by passing a custom interface
  (a wrapper function) to `find_source()`. Since this work on a lexical basis,
  the function does not have to be defined at the time `find_source()` is
  called.
    * Users may migrate to the new mechanism by binding deprecated symbol
      `translate` to a function that calls method `Translator$translate()`
      in the environment of their choice (where it *fits*).
* New option `transltr.verbose`. It can be used to set all `verbose` arguments
  to either `TRUE` or `FALSE`.
* New function `normalize()`. The latter used to be internal but it is now
  exported for convenience. It documents how strings are standardized.
* New method `Translator$set_default_value()`. It controls what methods
  `Translator$translate()` and `Translator$get_translation()` return when
  there is no translation.
* Documentation of `find_source()` and class `Translator` is now more thorough.
* Many details were added to the package's documentation overall. It was also
  simplified whenever appropriate.

## Bug Fixes

* `export.Text()` and `export_translations()` now wrap lines longer than 80
  characters automatically via new internal function `str_wrap()`.
  * Consequently, so does `serialize()` and `serialize_translations()`.
* `normalize()` now handles edge cases appropriately thanks to `stringi`.
* `format.Text()` and `format.Translator()` now escape newline characters.
* Identifiers of serialized `Location` objects embedded into serialized `Text`
  objects are now much shorter.
* `find_source()` uses relative paths (to the project's working directory) by
  default.

## Notes

* The official title of the package (as it appears in `DESCRIPTION`) is now
  *Support Many Languages in R*.
* The package now has a `pkgdown` website. See <https://transltr.ununoctium.dev>.
  It is not yet complete and requires more work.
* A lot of typos were corrected here and there in the documentation.
* Comments included in Translator and translations files were updated.
* In outputs, `Source Text` is now used (it used to be pluralized).
* The source code was (lightly) tidied.

## Translations

The package is not yet translated.

# `transltr` 0.0.1

This is the first release of the package. While an extensive set of unit tests
fully covers it, some features could be modified in the future. Treat it as a
beta version until version 1.0.0 is released.
