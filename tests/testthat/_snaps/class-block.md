# block() validates argument hash

    Code
      block(1L)
    Condition
      Error:
      ! 'hash' must be a non-NA and non-empty character of length 1.

# block() validates argument text

    Code
      block(hash, text = 1L)
    Condition
      Error:
      ! 'text' must be a non-NA and non-empty character of length 1.

# block() validates argument text_key

    Code
      block(hash, text, text_key = 1L)
    Condition
      Error:
      ! 'text_key' must be a non-NA and non-empty character of length 1.

# block() validates argument locations

    Code
      block(hash, text, key, locations = 1L)
    Condition
      Error:
      ! 'locations' must be a non-empty list.

---

    Code
      block(hash, text, key, locations = list(1L))
    Condition
      Error:
      ! 'locations' must only contain 'Location' objects.

# block() validates argument translations

    Code
      block(hash, text, key, locs, 1L)
    Condition
      Error:
      ! 'translations' must be a non-empty character vector of non-NA values.

---

    Code
      block(hash, text, key, locs, unname(trans))
    Condition
      Error:
      ! 'translations' must have names.

# print() works

    Code
      print(block)
    Output
      <Block>
        Hash     : test-hash
        Text     : Hello, world!
        Text key : en
        Lang keys: 'en', 'fr', 'es'
        <Location>
          'file1':
            - line 1, column 2 @ line 3, column 4
        <Location>
          'file2':
            - line 1, column 3 @ line 5, column 7
            - line 2, column 4 @ line 6, column 8

