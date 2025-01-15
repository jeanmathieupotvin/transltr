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

# print() works

    Code
      print(loc1, "short")
    Output
      <Location>
       Path: tests/testthat/my-test-file
       Ranges: Ln 1, Col 2 @ Ln 3, Col 4

---

    Code
      print(loc2, "long")
    Output
      <Location>
       Path: tests/testthat/my-test-file
       Ranges:
        Ln  1, Col  22 @ Ln   10, Col 1
        Ln 11, Col 222 @ Ln 3333, Col 4

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
      ! all 'path' must be equal in order to combine 'Location' objects.

# merge_locations() validates ...

    Code
      merge_locations(loc1, 1L, loc2)
    Condition
      Error:
      ! values passed to '...' must all be 'Location' objects.

# range_format() validates x

    Code
      range_format(1L)
    Condition
      Error:
      ! 'x' must be a 'Location' object.

# range_parse() validates strings

    Code
      range_parse(1L)
    Condition
      Error:
      ! 'ranges' must be a non-empty character vector of non-NA values.

# range_is_parseable() validates strings

    Code
      range_is_parseable(1L)
    Condition
      Error:
      ! 'ranges' must be a non-empty character vector of non-NA values.

