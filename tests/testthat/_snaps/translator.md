# translator_set() validates x

    Code
      translator_set(1L)
    Condition
      Error:
      ! 'x' must be a 'Translator' object.

# translator_set() validates scope

    Code
      translator_set(scope = 1L)
    Condition
      Error:
      ! 'scope' must be a non-NA and non-empty character of length 1.

# translator_get() validates scope

    Code
      translator_get(1L)
    Condition
      Error:
      ! 'scope' must be a non-NA and non-empty character of length 1.

