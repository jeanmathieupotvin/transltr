# language_set() validates key

    Code
      language_set(1L)
    Condition
      Error:
      ! 'key' must be a non-NA and non-empty character of length 1.

# language_set() throws an error if it fails to set environment variable

    Code
      # .__LGL_DEBUG_FLAG was set equal to TRUE to generate this error.
      language_set("test-en")
    Condition
      Error:
      ! failed to set language key 'test-en'.

# language_set() throws an error if it fails to unset environment variable

    Code
      # .__LGL_DEBUG_FLAG was set equal to TRUE to generate this error.
      language_set(NULL)
    Condition
      Error:
      ! failed to unset current language key.

