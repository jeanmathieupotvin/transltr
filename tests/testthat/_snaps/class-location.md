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
      ! 'line1' must be a non-NA integer of length 1.

# location() validates argument col1

    Code
      location(col1 = "")
    Condition
      Error:
      ! 'col1' must be a non-NA integer of length 1.

# location() validates argument line2

    Code
      location(line2 = "")
    Condition
      Error:
      ! 'line2' must be a non-NA integer of length 1.

# location() validates argument col2

    Code
      location(col2 = "")
    Condition
      Error:
      ! 'col2' must be a non-NA integer of length 1.

# print() works

    Code
      print(test_loc)
    Output
      <Location> tests/testthat/my-test-file: ln 1, col 2 @ ln 3, col 4

