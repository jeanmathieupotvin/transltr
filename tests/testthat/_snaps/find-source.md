# find_source() validates path

    Code
      find_source(1L)
    Condition
      Error:
      ! 'path' must be a non-NA and non-empty character of length 1.

---

    Code
      find_source("non-existent-directory")
    Condition
      Error:
      ! 'path' does not exist or is not a directory.

# find_source() validates native_languages

    Code
      find_source(native_languages = 1L)
    Condition
      Error:
      ! 'native_languages' must be a character vector of non-NA values.

---

    Code
      find_source(native_languages = "English")
    Condition
      Error:
      ! 'native_languages' must have names.

# find_source_in_files() validates paths

    Code
      find_source_in_files(1L)
    Condition
      Error:
      ! 'paths' must be a non-empty character vector of non-NA values.

# find_source_in_files() validates strict

    Code
      find_source_in_files(path_mock1, strict = 1L)
    Condition
      Error:
      ! 'strict' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# find_source_in_files() validates algorithm

    Code
      find_source_in_files(path_mock1, algorithm = 1L)
    Condition
      Error:
      ! 'algorithm' must be equal to 'sha1', or 'utf8'.

# find_source_in_files() validates verbose

    Code
      find_source_in_files(path_mock1, verbose = 1L)
    Condition
      Error:
      ! 'verbose' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

