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

