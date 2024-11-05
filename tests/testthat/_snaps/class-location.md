# location() validates path

    Code
      location(1L)
    Condition
      Error:
      ! 'path' must be a non-NA and non-empty character of length 1.

# location() validates line1

    Code
      location(line1 = "")
    Condition
      Error:
      ! 'line1' must be a non-empty integer vector of non-NA values.

---

    Code
      location(line1 = 0L)
    Condition
      Error:
      ! all values passed to 'line1', 'col1', 'line2', and 'col2' must be non-NA numeric values in the range [1, Inf).

# location() validates col1

    Code
      location(col1 = "")
    Condition
      Error:
      ! 'col1' must be a non-empty integer vector of non-NA values.

---

    Code
      location(col1 = 0L)
    Condition
      Error:
      ! all values passed to 'line1', 'col1', 'line2', and 'col2' must be non-NA numeric values in the range [1, Inf).

# location() validates line2

    Code
      location(line2 = "")
    Condition
      Error:
      ! 'line2' must be a non-empty integer vector of non-NA values.

---

    Code
      location(line2 = 0L)
    Condition
      Error:
      ! all values passed to 'line1', 'col1', 'line2', and 'col2' must be non-NA numeric values in the range [1, Inf).

# location() validates col2

    Code
      location(col2 = "")
    Condition
      Error:
      ! 'col2' must be a non-empty integer vector of non-NA values.

---

    Code
      location(col2 = 0L)
    Condition
      Error:
      ! all values passed to 'line1', 'col1', 'line2', and 'col2' must be non-NA numeric values in the range [1, Inf).

# location() validates line1, col1, line2, and col2 have the same length

    Code
      location(line1 = c(1L, 2L))
    Condition
      Error:
      ! line1', 'col1', 'line2', and 'col2' must all have the same length.

# format() validates how

    Code
      format(location(), "error")
    Condition
      Error:
      ! 'how' must be equal to 'long', or 'short'.

# .format_short_location() throws an error if multiple ranges must be printed

    Code
      format(loc2, "short")
    Condition
      Error:
      ! 'line1', 'col1', 'line2', and 'col2' must all have a length equal to 1 in order to use the 'short' format.

# print() works

    Code
      print(loc1, "short")
    Output
      tests/testthat/my-test-file: ln 1, col 2 @ ln 3, col 4

---

    Code
      print(loc2, "long")
    Output
      <Location>
        Path: tests/testthat/my-test-file
        Ranges: 
          line  1, column  22 @ line   10, column 1
          line 11, column 222 @ line 3333, column 4

# c.Location() validates ...

    Code
      c(loc1, 1L, loc2)
    Condition
      Error:
      ! values passed to '...' must all be 'Location' objects.

# c.Location() throws an error if paths are not equal

    Code
      c(location("a"), location("b"))
    Condition
      Error:
      ! all 'path' must be equal in order to combine multiple 'Location' objects.

# merge_locations() validates ...

    Code
      merge_locations(loc1, 1L, loc2)
    Condition
      Error:
      ! values passed to '...' must all be 'Location' objects.

