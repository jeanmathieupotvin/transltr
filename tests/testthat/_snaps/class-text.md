# active binding hash throws an error if value is not missing

    Code
      txt1$hash <- "new-hash"
    Condition
      Error:
      ! 'hash' cannot be overwritten.
      Update it by setting 'source_lang' instead.

# active binding hash_algorithm validates value

    Code
      txt1$hash_algorithm <- 1L
    Condition
      Error:
      ! 'hash_algorithm' must be a non-NA and non-empty character of length 1.

---

    Code
      txt1$hash_algorithm <- "new-algo"
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'sha1', or 'utf8'.

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

# $initialize() validates hash_algorithm

    Code
      Text$new(1L)
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'sha1', or 'utf8'.

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
          <Location>
            Path: a
            Ranges: line 1, column 2 @ line 3, column 4
          <Location>
            Path: b
            Ranges: line 5, column 6 @ line 7, column 8

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
      ! all 'hash' must be equal in order to combine multiple 'Text' objects.

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

# merge_texts() validates hash_algorithm

    Code
      merge_texts(txt1, txt2, hash_algorithm = "error")
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'sha1', or 'utf8'.

# as_text.call() validates x

    Code
      as_text(call("text"))
    Condition
      Error:
      ! 'x' must be a 'call' object to 'transltr::translate()'.

# as_text.call() validates strict

    Code
      as_text(translate_call, strict = 1L)
    Condition
      Error:
      ! 'strict' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# as_text.call() validates location

    Code
      as_text(translate_call, location = 1L)
    Condition
      Error:
      ! 'location' must be a 'Location' object.

# as_text.call() validates validate

    Code
      as_text(translate_call, validate = 1L)
    Condition
      Error:
      ! 'validate' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').
