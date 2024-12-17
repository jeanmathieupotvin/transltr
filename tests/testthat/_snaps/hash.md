# hash() validates lang

    Code
      hash(1L)
    Condition
      Error:
      ! 'lang' must be a non-NA and non-empty character of length 1.

# hash() validates text

    Code
      hash("a", 1L)
    Condition
      Error:
      ! 'text' must be a non-NA character of length 1.

