# it returns a character vector

    Code
      cat(format_vector(x1, "<Test: x1>"), sep = "\n")
    Output
      <Test: x1>:
      FirstName: John
      LastName: Doe
      Address:
       StreetAddress: 123 Main Street
       City: Montreal
       Province: Quebec
       PostalCode: H0H 0H0
      Notes:
       Send mail to
       address above.

---

    Code
      cat(sep = "\n", format_vector(x2, label = "<Test: x2>", indent = 2L,
        fill_names = TRUE))
    Output
      <Test: x2>:
      level1: 0
      level2:
        a: 1
        b: 2
        [3]: 3
        level3:
          c: 4
          d: 5
          e: <empty> [list]
      level1:
        f: 6
        g: 7
      level1:
        h: <null>
        i: 8
        j: 9

# it validates validate

    Code
      format_vector(validate = 1L)
    Condition
      Error:
      ! 'validate' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# it validates label if validate is true

    Code
      format_vector(label = 1L)
    Condition
      Error:
      ! 'label' must be a non-NA and non-empty character of length 1, or 'NULL'.

# it validates level if validate is true

    Code
      format_vector(level = "1")
    Condition
      Error:
      ! 'level' must be a non-NA integer of length 1.

---

    Code
      format_vector(level = -1L)
    Condition
      Error:
      ! 'level' must be a non-NA numeric value in the range [0, Inf).

# it validates indent if validate is true

    Code
      format_vector(indent = " ")
    Condition
      Error:
      ! 'indent' must be a non-NA integer of length 1.

---

    Code
      format_vector(indent = -1L)
    Condition
      Error:
      ! 'indent' must be a non-NA numeric value in the range [0, Inf).

# it validates fill_names if validate is true

    Code
      format_vector(fill_names = 1L)
    Condition
      Error:
      ! 'fill_names' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# it validates null if validate is true

    Code
      format_vector(null = 1L)
    Condition
      Error:
      ! 'null' must be a non-NA and non-empty character of length 1.

# it validates empty if validate is true

    Code
      format_vector(empty = 1L)
    Condition
      Error:
      ! 'empty' must be a non-NA and non-empty character of length 1.

# it replaces empty objects by empty

    Code
      # An example of format_vector() replacing empty objects.
      # See the underlying test block to inspect the input.
      cat(out, sep = "\n")
    Output
      a: <empty> [logical]
      b: <empty> [integer]
      c: <empty> [double]
      d: <empty> [complex]
      e: <empty> [character]
      f: <empty> [raw]
      g: <empty> [list]
      h: <null>
      i:
       j: <empty> [logical]
       k: <empty> [integer]
       l: <empty> [double]
       m: <empty> [complex]
       n: <empty> [character]
       o: <empty> [raw]
       p: <empty> [list]
       q: <null>

# it replaces empty character strings by pairs of double quotes

    Code
      # An example of format_vector() replacing empty character strings.
      # See the underlying test block to inspect the input.
      cat(out, sep = "\n")
    Output
      a: a
      b: ""
      c: c

