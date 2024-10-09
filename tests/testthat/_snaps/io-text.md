# read_text() validates path

    Code
      read_text(1L)
    Condition
      Error:
      ! 'path' must be a non-NA and non-empty character of length 1.

---

    Code
      read_text("a-non-existent-file.txt")
    Condition
      Error:
      ! 'path' does not exist, is a directory, or is not readable.

# read_text() validates encoding

    Code
      read_text(mock_file_utf8_path, "")
    Condition
      Error:
      ! 'encoding' must be a non-NA and non-empty character of length 1.

