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
