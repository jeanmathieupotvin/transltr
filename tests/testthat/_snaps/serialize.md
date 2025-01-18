# export.Location() validates id

    Code
      export(loc1, 1L)
    Condition
      Error:
      ! 'id' must be a non-NA and non-empty character of length 1.

# export.Text() validates id

    Code
      export(txt1, 1L)
    Condition
      Error:
      ! 'id' must be a non-NA and non-empty character of length 1.

# export.Text() validates set_translations

    Code
      export(txt1, set_translations = 1L)
    Condition
      Error:
      ! 'set_translations' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# serialize() serializes objects as expected

    Code
      cat(serialize(loc1, id = "test-id"))
    Output
      !<Location>
      Identifier: test-id
      Path: a
      Ranges: Ln 1, Col 2 @ Ln 3, Col 4

---

    Code
      cat(serialize(txt1, id = "test-id"))
    Output
      !<Text>
      Identifier: test-id
      Algorithm: sha1
      Hash: 256e0d707386d0fcd9abf10ad994000bdaa25812
      Source Language: en
      Source Text: Hello, world!
      Translations: ~
      Locations:
        - !<Location>
          Identifier: test-id:a
          Path: a
          Ranges: Ln 1, Col 2 @ Ln 3, Col 4

---

    Code
      cat(serialize(tr))
    Output
      !<Translator>
      Identifier: test-translator
      Algorithm: sha1
      Languages:
        el: Ελληνικά
        en: English
        es: Español
        fr: Français
      Texts:
        - !<Text>
          Identifier: 256e0d7
          Algorithm: sha1
          Hash: 256e0d707386d0fcd9abf10ad994000bdaa25812
          Source Language: en
          Source Text: Hello, world!
          Translations: ~
          Locations:
            - !<Location>
              Identifier: 256e0d7:a
              Path: a
              Ranges: Ln 1, Col 2 @ Ln 3, Col 4
        - !<Text>
          Identifier: 2ac373a
          Algorithm: sha1
          Hash: 2ac373aa699a6712cdaddbead28031d537de29bc
          Source Language: en
          Source Text: Farewell, world!
          Translations: ~
          Locations:
            - !<Location>
              Identifier: 2ac373a:b
              Path: b
              Ranges: Ln 5, Col 6 @ Ln 7, Col 8

# export_translations() validates lang

    Code
      export_translations(tr, 1L)
    Condition
      Error:
      ! 'lang' must be a non-NA and non-empty character of length 1.

---

    Code
      export_translations(tr, "missing-lang")
    Condition
      Error:
      ! 'lang' must have a corresponding native language registered in 'tr'.

# export_translations() validates tr

    Code
      export_translations(1L, "fr")
    Condition
      Error:
      ! 'tr' must be a 'Translator' object.

---

    Code
      export_translations(tr, "fr")
    Condition
      Error:
      ! all 'Text' objects of 'tr' must have the same 'source_lang'.

# serialize_translations() serializes translations as expected

    Code
      cat(serialize_translations(tr, "el"), "\n")
    Output
      :: Identifier
      
      test-translator
      
      :: Language Code
      
      el
      
      :: Language
      
      Ελληνικά
      
      :: Source Language
      
      English
      
      :: Translations: 256e0d7: Source Text
      
      Hello, world!
      
      :: Translations: 256e0d7: Translation
      
      # Insert a translation here.
      
      :: Translations: 2ac373a: Source Text
      
      Farewell, world!
      
      :: Translations: 2ac373a: Translation
      
      # Insert a translation here. 

---

    Code
      cat(serialize_translations(tr, "fr"), "\n")
    Output
      :: Identifier
      
      test-translator
      
      :: Language Code
      
      fr
      
      :: Language
      
      Français
      
      :: Source Language
      
      English
      
      :: Translations: 256e0d7: Source Text
      
      Hello, world!
      
      :: Translations: 256e0d7: Translation
      
      Bonjour, monde!
      
      :: Translations: 2ac373a: Source Text
      
      Farewell, world!
      
      :: Translations: 2ac373a: Translation
      
      Au revoir, monde! 

# format_errors() validates errors

    Code
      format_errors(1L)
    Condition
      Error:
      ! 'errors' must be a non-empty character vector of non-NA values.

# format_errors() validates throw_error

    Code
      format_errors("", throw_error = 1L)
    Condition
      Error:
      ! 'throw_error' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# format_errors() formats errors as expected

    Code
      format_errors(errors, "test-id")
    Condition
      Error:
      ! in object 'test-id':
       - 'Hash' must be a null, or a non-empty character string.
       - 'Hash' is defined but not 'Source Text', and/or 'Source Lang'.

# assert.ExportedLocation() throws an error if x is invalid and throw_error is true

    Code
      assert(invalid)
    Condition
      Error:
      ! in object '<unknown>':
       - 'Path' must be a non-empty character string.
       - 'Ranges' must be a single `Ln <int>, Col <int> @ Ln <int>, Col <int>` character string, or a sequence of such values.

# assert.ExportedLocation() detects invalid Path field

    Code
      assert(out)
    Condition
      Error:
      ! in object 'test-id':
       - 'Path' must be a non-empty character string.

# assert.ExportedLocation() detects invalid Ranges field

    Code
      assert(out)
    Condition
      Error:
      ! in object 'test-id':
       - 'Ranges' must be a single `Ln <int>, Col <int> @ Ln <int>, Col <int>` character string, or a sequence of such values.

# assert.ExportedText() throws an error if x is invalid and throw_error is true

    Code
      assert(invalid)
    Condition
      Error:
      ! in object '<unknown>':
       - 'Algorithm' must be equal to 'sha1', or 'utf8'.
       - 'Hash' must be a null, or a non-empty character string.
       - 'Hash' is defined but not 'Source Text', and/or 'Source Lang'.
       - 'Source Language' must be a null, or a non-empty character string.
       - 'Source Text' must be a null, or a non-empty character string.
       - 'Translations' must be a null, or a mapping of non-empty character strings.
       - 'Locations' must be a sequence of 'Location' objects.

# assert.ExportedText() detects invalid Algorithm field

    Code
      assert(out)
    Condition
      Error:
      ! in object 'test-id':
       - 'Algorithm' must be equal to 'sha1', or 'utf8'.

# assert.ExportedText() detects invalid Hash field

    Code
      assert(out1)
    Condition
      Error:
      ! in object 'test-id':
       - 'Hash' must be a null, or a non-empty character string.

---

    Code
      assert(out3)
    Condition
      Error:
      ! in object 'test-id':
       - 'Hash' is defined but not 'Source Text', and/or 'Source Lang'.

# assert.ExportedText() detects invalid Source Language field

    Code
      assert(out1)
    Condition
      Error:
      ! in object 'test-id':
       - 'Hash' is defined but not 'Source Text', and/or 'Source Lang'.
       - 'Source Language' must be a null, or a non-empty character string.
       - 'Source Language' is defined but not 'Source Text', or vice-versa.

# assert.ExportedText() detects invalid Source Text field

    Code
      assert(out1)
    Condition
      Error:
      ! in object 'test-id':
       - 'Hash' is defined but not 'Source Text', and/or 'Source Lang'.
       - 'Source Text' must be a null, or a non-empty character string.
       - 'Source Language' is defined but not 'Source Text', or vice-versa.

---

    Code
      assert(out2)
    Condition
      Error:
      ! in object 'test-id':
       - 'Source Text' must be a null, or a non-empty character string.

# assert.ExportedText() detects invalid Translations field

    Code
      assert(out1)
    Condition
      Error:
      ! in object 'test-id':
       - 'Translations' must be a null, or a mapping of non-empty character strings.

# assert.ExportedText() detects invalid Locations field

    Code
      assert(out1)
    Condition
      Error:
      ! in object 'test-id':
       - 'Locations' must be a sequence of 'Location' objects.

---

    Code
      assert(out3)
    Condition
      Error:
      ! in object 'test-id':
       - ['test-id'] 'Path' must be a non-empty character string.
       - ['test-id'] 'Ranges' must be a single `Ln <int>, Col <int> @ Ln <int>, Col <int>` character string, or a sequence of such values.

# assert.ExportedTranslator() throws an error if x is invalid and throw_error is true

    Code
      assert(invalid)
    Condition
      Error:
      ! in object '<unknown>':
       - 'Identifier' must be a non-empty character string.
       - 'Algorithm' must be equal to 'sha1', or 'utf8'.
       - 'Languages' must a mapping of non-empty character strings.
       - 'Texts' must a sequence of 'Text' objects.

# assert.ExportedTranslator() detects invalid Identifier field

    Code
      assert(out)
    Condition
      Error:
      ! in object '1':
       - 'Identifier' must be a non-empty character string.

# assert.ExportedTranslator() detects invalid Algorithm field

    Code
      assert(out)
    Condition
      Error:
      ! in object 'test-translator':
       - 'Algorithm' must be equal to 'sha1', or 'utf8'.

# assert.ExportedTranslator() detects invalid Languages field

    Code
      assert(out1)
    Condition
      Error:
      ! in object 'test-translator':
       - 'Languages' must a mapping of non-empty character strings.

# assert.ExportedTranslator() detects invalid Texts field

    Code
      assert(out1)
    Condition
      Error:
      ! in object 'test-translator':
       - 'Texts' must a sequence of 'Text' objects.

---

    Code
      assert(out3)
    Condition
      Error:
      ! in object 'test-translator':
       - ['test-id'] 'Hash' must be a null, or a non-empty character string.
       - ['test-id'] 'Algorithm' must be equal to 'sha1', or 'utf8'.

