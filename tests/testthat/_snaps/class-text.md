# active binding hash throws an error if value is not missing

    Code
      txt1$hash <- "new-hash"
    Condition
      Error:
      ! 'hash' cannot be overwritten.
      Update it by setting 'source_lang' instead.

# active binding algorithm validates value

    Code
      txt1$algorithm <- 1L
    Condition
      Error:
      ! 'algorithm' must be equal to 'sha1', or 'utf8'.

---

    Code
      txt1$algorithm <- "new-algo"
    Condition
      Error:
      ! 'algorithm' must be equal to 'sha1', or 'utf8'.

# active binding source_lang validates value

    Code
      txt1$source_lang <- 1L
    Condition
      Error:
      ! 'source_lang' must be a non-NA and non-empty character of length 1.

---

    Code
      txt1$source_lang <- "new-lang"
    Condition
      Error:
      ! 'source_lang' must be equal to 'en', 'es', 'fr', or 'ja'.

# active binding source_text throws an error if value is not missing

    Code
      txt1$source_text <- "new-text"
    Condition
      Error:
      ! 'source_text' cannot be overwritten.
      Update it by setting 'source_lang'.
      You may add a new translation before doing so.

# active binding languages throws an error if value is not missing

    Code
      txt1$languages <- "new-lang"
    Condition
      Error:
      ! 'languages' cannot be overwritten.
      Update them by setting, or removing translations.

# active binding translations throws an error if value is not missing

    Code
      txt1$translations <- "new-translation"
    Condition
      Error:
      ! 'translations' cannot be overwritten.
      Update them by setting, or removing translations.

# active binding locations throws an error if value is not missing

    Code
      txt1$locations <- location()
    Condition
      Error:
      ! 'locations' cannot be overwritten.
      Update them by setting, or removing 'Location' objects.

# $get_translation() validates lang

    Code
      txt1$get_translation(1L)
    Condition
      Error:
      ! 'lang' must be a non-NA and non-empty character of length 1.

# $set_translation() validates lang

    Code
      txt1$set_translation(1L)
    Condition
      Error:
      ! 'lang' must be a non-NA and non-empty character of length 1.

# $set_translation() validates text

    Code
      txt1$set_translation("de", 1L)
    Condition
      Error:
      ! 'text' must be a non-NA character of length 1.

# $set_translations() validates ...

    Code
      txt$set_translations(1L)
    Condition
      Error:
      ! values passed to '...' must all be non-NA and non-empty character strings.

---

    Code
      txt$set_translations("Hello, world!")
    Condition
      Error:
      ! '...' must have names.

# $rm_translation() validates lang

    Code
      txt1$rm_translation(1L)
    Condition
      Error:
      ! 'lang' must be a non-NA and non-empty character of length 1.

---

    Code
      txt1$rm_translation("en")
    Condition
      Error:
      ! 'en' is the current 'source_lang'. Set a new one before removing it.

---

    Code
      txt1$rm_translation("error")
    Condition
      Error:
      ! 'lang' must be equal to 'es', 'fr', or 'ja'.

# $rm_location() validates path

    Code
      txt1$rm_location(1L)
    Condition
      Error:
      ! 'path' must be a non-NA and non-empty character of length 1.

---

    Code
      txt1$rm_location("error")
    Condition
      Error:
      ! 'path' must be equal to 'a', or 'b'.

# text() validates source_lang

    Code
      text(source_lang = "")
    Condition
      Error:
      ! 'source_lang' must be a non-NA and non-empty character of length 1.

# text() checks that there is at least one translation corresponding to source_lang

    Code
      text()
    Condition
      Error:
      ! a translation corresponding to 'source_lang' must be passed to '...'.
      It is treated as the source text.

# format() sets names of locations equal to base names

    Code
      print(txt)
    Output
      <Text>
       Hash: 256e0d707386d0fcd9abf10ad994000bdaa25812
       Source Lang: en
       Algorithm: sha1
       Translations:
        el: Γεια σου, Κόσμος!
        en: Hello, world!
       Locations:
        c:
         <Location>
          Path: /absolute/path/to/source/script/c
          Ranges: Ln 1, Col 2 @ Ln 3, Col 4
        d:
         <Location>
          Path: /absolute/path/to/source/script/d
          Ranges: Ln 5, Col 6 @ Ln 7, Col 8

# format() escapes newlines

    Code
      print(txt)
    Output
      <Text>
       Hash: b3f088a87ffeeccce4cf1ccc64e34419fd736ae2
       Source Lang: en
       Algorithm: sha1
       Translations:
        el: Γεια σου,\n\nΚόσμος!
        en: Hello,\n\nworld!
       Locations: <empty> [list]

# print() works

    Code
      print(txt1)
    Output
      <Text>
       Hash: 256e0d707386d0fcd9abf10ad994000bdaa25812
       Source Lang: en
       Algorithm: sha1
       Translations:
        en: Hello, world!
        es: ¡Hola Mundo!
        fr: Bonjour, monde!
        ja: こんにちは世界！
       Locations:
        a:
         <Location>
          Path: a
          Ranges: Ln 1, Col 2 @ Ln 3, Col 4
        b:
         <Location>
          Path: b
          Ranges: Ln 5, Col 6 @ Ln 7, Col 8

# c.Text() validates ...

    Code
      c(txt1, 1L, txt2)
    Condition
      Error:
      ! values passed to '...' must all be 'Text' objects.

# c.Text() throws an error if hashes are not equal

    Code
      txt1 <- test_text()
      txt2 <- test_text()
      txt2$source_lang <- "fr"
      c(txt1, txt2)
    Condition
      Error:
      ! all 'hash' must be equal in order to combine 'Text' objects.

# c.Text() throws an error if source languages are not set

    Code
      c(Text$new(), Text$new())
    Condition
      Error:
      ! all 'Text' objects have no source language set.

# merge_texts() validates ...

    Code
      merge_texts(txt1, 1L, txt2)
    Condition
      Error:
      ! values passed to '...' must all be 'Text' objects.

# merge_texts() validates algorithm

    Code
      merge_texts(txt1, txt2, algorithm = "error")
    Condition
      Error:
      ! 'algorithm' must be equal to 'sha1', or 'utf8'.

# as_text.call() validates loc

    Code
      as_text(translate_call, loc = 1L)
    Condition
      Error:
      ! 'loc' must be a 'Location' object.

