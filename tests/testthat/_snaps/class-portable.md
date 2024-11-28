# portable() validates super

    Code
      portable(1L, super = 1L)
    Condition
      Error:
      ! 'super' must be a character vector of non-NA values.

# portable() validates tag if not null

    Code
      portable(1L, tag = 1L)
    Condition
      Error:
      ! 'tag' must be a non-NA and non-empty character of length 1.

# portable_translator() validates x

    Code
      portable_translator(1L)
    Condition
      Error:
      ! 'x' must be a 'Translator' object.

# portable_translator() throws an error if there are multiple source languages

    Code
      portable_translator(trans)
    Condition
      Error:
      ! all 'Text' objects of 'x' must have the same 'source_lang'.

# portable_text() validates x

    Code
      portable_text(1L)
    Condition
      Error:
      ! 'x' must be a 'Text' object.

# portable_text() validates set_translations

    Code
      portable_text(Text$new(), 1L)
    Condition
      Error:
      ! 'set_translations' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# portable_text() wraps source text and translations

    Code
      out$source_text
    Output
      [1] "Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,\nwhen an unknown printer took a galley of type and scrambled it to make a type\nspecimen book."
    Code
      out$translations$fr
    Output
      [1] "Le Lorem Ipsum est le faux texte standard de l'imprimerie depuis les années\n1500, quand un imprimeur anonyme assembla ensemble des morceaux de texte pour\nréaliser un livre spécimen de polices de texte."

# portable_location() validates x

    Code
      portable_location(1L)
    Condition
      Error:
      ! 'x' must be a 'Location' object.

# format.PortableTranslator() validates set_instructions

    Code
      format(portable_trans, 1L)
    Condition
      Error:
      ! 'set_instructions' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# print() works

    Code
      # Manually check if output below is a YAML serialization.
      # Output should:
      #   1. begin with a !<Person> YAML tag,
      #   2. Use a key: value notation,
      #   3. Use unquoted strings (if possible), and
      #   4. Use indentation to indicate structure.
      # If these 4 characteristics are observed, the
      # output is highly likely to be a YAML string.
      print(portable_person)
    Output
      !<Person>
      FirstName: John
      LastName: Doe
      Address:
        StreetAddress: 123 Main Street
        City: Montreal
        Province: Quebec
        PostalCode: H0H 0H0
      Notes:
        - Send mail to
        - address above.
      

# print() works with PortableTranslator objects

    Code
      print(portable_translator(Translator$new(nil_uuid)))
    Output
      !<Translator>
      version: 1
      generated_by: R package transltr 0.0.1
      generated_on: January 01, 1900 @ 00:00:00 (UTC)
      identifier: 00000000-0000-0000-0000-000000000000
      hash_algorithm: sha1
      source_language: ~
      languages: []
      translations_files: !<TranslationsFiles> []
      

---

    Code
      print(portable_trans, TRUE)
    Output
      # Portable Translator
      #
      # Instructions:
      #  - You may edit fields identifier, languages, and translations_files.
      #  - Do not edit other fields by hand; edit source scripts instead.
      #  - Translations are stored in separate files. See translations_files.
      %YAML 1.1
      ---
      !<Translator>
      version: 1
      generated_by: R package transltr 0.0.1
      generated_on: January 01, 1900 @ 00:00:00 (UTC)
      identifier: 00000000-0000-0000-0000-000000000000
      hash_algorithm: sha1
      source_language: en
      languages:
        en: English
        fr: Français
      translations_files: !<TranslationsFiles>
        fr: fr.txt
      256e0d7: !<Text>
        hash: 256e0d707386d0fcd9abf10ad994000bdaa25812
        hash_algorithm: sha1
        source_language: en
        source_text: Hello, world!
        translations: ~
        locations:
          - !<Location>
            path: a
            ranges: line 1, column 2 @ line 3, column 4
      2ac373a: !<Text>
        hash: 2ac373aa699a6712cdaddbead28031d537de29bc
        hash_algorithm: sha1
        source_language: en
        source_text: Farewell, world!
        translations: ~
        locations:
          - !<Location>
            path: b
            ranges: line 1, column 2 @ line 3, column 4
      

# print() works with PortableText objects

    Code
      print(portable_text(Text$new()))
    Output
      !<Text>
      hash: ~
      hash_algorithm: sha1
      source_language: ~
      source_text: ~
      translations: ~
      locations: []
      

---

    Code
      print(portable_txt)
    Output
      !<Text>
      hash: '12351'
      hash_algorithm: utf8
      source_language: en
      source_text: Hello, world!
      translations: ~
      locations:
        - !<Location>
          path: test-file-1
          ranges: line 1, column 2 @ line 3, column 4
        - !<Location>
          path: test-file-2
          ranges: line 1, column 2 @ line 3, column 4
      

# print() works with PortableLocation objects

    Code
      print(portable_loc)
    Output
      !<Location>
      path: test-file
      ranges: line 1, column 2 @ line 3, column 4
      

