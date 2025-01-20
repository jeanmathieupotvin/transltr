# text_read() validates path

    Code
      text_read(1L)
    Condition
      Error:
      ! 'path' must be a non-NA and non-empty character of length 1.

---

    Code
      text_read("a-non-existent-file.txt")
    Condition
      Error:
      ! 'path' 'a-non-existent-file.txt' does not exist, is a directory, or is not readable.

# text_read() validates encoding

    Code
      text_read(mock_file_utf8_path, "")
    Condition
      Error:
      ! 'encoding' must be a non-NA and non-empty character of length 1.

# text_write() validates x

    Code
      text_write(1L)
    Condition
      Error:
      ! 'x' must be a non-empty character vector of non-NA values.

# text_write() validates path

    Code
      text_write("Hello, world!", 1L)
    Condition
      Error:
      ! 'path' must be a non-NA and non-empty character of length 1.

---

    Code
      # Input temporary directory is random. It is replaced
      # by a constant for in this snapshot for reproducibility.
      text_write("Hello, world!", temp_dir)
    Condition
      Error:
      ! 'path' 'a-test-directory' is a directory, or is not writable.

# text_write() validates encoding

    Code
      text_write("Hello, world!", temp_file, 1L)
    Condition
      Error:
      ! 'encoding' must be a non-NA and non-empty character of length 1.

