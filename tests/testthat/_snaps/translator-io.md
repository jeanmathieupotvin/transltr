# translations_paths() validates tr

    Code
      translations_paths(1L)
    Condition
      Error:
      ! 'tr' must be a 'Translator' object.

---

    Code
      translations_paths(tr)
    Condition
      Error:
      ! all 'Text' objects of 'tr' must have the same 'source_lang'.

# translations_paths() validates parent_dir

    Code
      translations_paths(tr, 1L)
    Condition
      Error:
      ! 'parent_dir' must be a non-NA and non-empty character of length 1.

# translator_write() validates path

    Code
      translator_write(tr, 1L)
    Condition
      Error:
      ! 'path' must be a non-NA and non-empty character of length 1.

# translator_write() validates overwrite

    Code
      translator_write(tr, overwrite = 1L)
    Condition
      Error:
      ! 'overwrite' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# translator_write() validates verbose

    Code
      translator_write(tr, verbose = 1L)
    Condition
      Error:
      ! 'verbose' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# translator_write() validates translations

    Code
      translator_write(tr, translations = 1L)
    Condition
      Error:
      ! 'translations' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# translator_write() throws an error if path exists and overwrite is false

    Code
      translator_write(tr, temp_file, FALSE)
    Condition
      Error:
      ! 'path' already exists. Set 'overwrite' equal to 'TRUE'.

# translator_write() throws an error if parent directories cannot be created

    Code
      translator_write(tr, temp_file)
    Condition
      Error:
      ! parent directory of 'path' could not be created.

# translator_write() outputs basic information if verbose is true

    Code
      translator_write(tr, temp_file, TRUE)
    Output
      Writing 'el' translations to './el.txt'.
      Writing 'es' translations to './es.txt'.
      Writing 'fr' translations to './fr.txt'.

# translator_read() validates verbose

    Code
      translator_read(verbose = 1L)
    Condition
      Error:
      ! 'verbose' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# translator_read() validates translations

    Code
      translator_read(translations = 1L)
    Condition
      Error:
      ! 'translations' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# translator_read() outputs basic information if verbose is true

    Code
      translator_read(temp_file)
    Output
      Reading translations from './el.txt'.
      Reading translations from './es.txt'.
      Reading translations from './fr.txt'.
      <Translator>
       Identifier: test-translator
       Algorithm: sha1
       Languages:
        el: Ελληνικά
        en: English
        es: Español
        fr: Français
       Source Text:
        256e0d7 [en, es, fr]: Hello, world!
        2ac373a [en, fr]: Farewell, world!

# translator_read() reports errors

    Code
      translator_read(temp_file, verbose = TRUE)
    Output
      Reading translations from './el.txt'.
      Error(s): 'Language' must be a non-empty character string.
      Reading translations from './es.txt'.
      Error(s): 'Language Code' must be a non-empty character string.
      Reading translations from './fr.txt'.
      Error(s): 
       - 'Language' must be a non-empty character string.
       - 'Source Language' must be a non-empty character string.
      <Translator>
       Identifier: test-translator
       Algorithm: sha1
       Languages:
        el: Ελληνικά
        en: English
        es: Español
        fr: Français
       Source Text:
        256e0d7 [en]: Hello, world!
        2ac373a [en]: Farewell, world!

---

    Code
      translator_read(temp_file, verbose = FALSE)
    Condition
      Error:
      ! in './el.txt': 'Language' must be a non-empty character string.

