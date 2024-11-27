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

# portable_location() validates x

    Code
      portable_location(1L)
    Condition
      Error:
      ! 'x' must be a 'Location' object.

