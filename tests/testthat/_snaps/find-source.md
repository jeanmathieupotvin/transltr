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

# find_source() validates tr

    Code
      find_source(tr = 1L)
    Condition
      Error:
      ! 'tr' must be a 'Translator' object.

# find_source_in_files() validates paths

    Code
      find_source_in_files(1L)
    Condition
      Error:
      ! 'paths' must be a non-empty character vector of non-NA values.

# find_source_in_files() validates verbose

    Code
      find_source_in_files(path_mock1, verbose = 1L)
    Condition
      Error:
      ! 'verbose' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# find_source_in_files() validates algorithm

    Code
      find_source_in_files(path_mock1, algorithm = 1L)
    Condition
      Error:
      ! 'algorithm' must be equal to 'sha1', or 'utf8'.

# find_source_in_files() validates interface

    Code
      find_source_in_files(path_mock1, interface = 1L)
    Condition
      Error:
      ! 'interface' must be a 'name', a 'call' object, or 'NULL'.
      Calls must be to operator `::`, i.e. 'pkg::fun'.

