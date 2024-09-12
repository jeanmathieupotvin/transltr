# location() validates argument path

    Code
      location(1L)
    Condition
      Error:
      ! 'path' must be a non-NA and non-empty character of length 1.

# location() validates argument line1

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

# location() validates argument col1

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

# location() validates argument line2

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

# location() validates argument col2

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
      print(test_loc)
    Output
      <Location>
        'tests/testthat/my-test-file':
          - line  1, column  22 @ line   10, column 1
          - line 11, column 222 @ line 3333, column 4

