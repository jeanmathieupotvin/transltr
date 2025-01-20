# This mock object contains a lot of comments (#)
# and escaped comments (\#) for testing purposes.
mock_flat_object_path <- get_mock_path(file.path("flat", "object"), "txt")


# flat_serialize() -------------------------------------------------------------


test_that("_serialize() returns a character string", {
    out <- flat_serialize(list(1L, 2L, 3L))

    expect_type(out, "character")
    expect_length(out, 1L)
})

test_that("_serialize() serializes empty lists to an empty character string", {
    expect_identical(flat_serialize(list()), "")
})

test_that("_serialize() serializes lists into streams of unindented sections beginning by ::", {
    expect_identical(
        flat_serialize(list(a = 1L, b = 2L, c = 3L)),
        ":: a\n\n1\n\n:: b\n\n2\n\n:: c\n\n3")
    expect_identical(
        flat_serialize(list(a = 1L, b = 2L, cd = list(c = 3L, d = 4L))),
        ":: a\n\n1\n\n:: b\n\n2\n\n:: cd: c\n\n3\n\n:: cd: d\n\n4")
    expect_identical(
        flat_serialize(list(a = 1L, b = 2L, cd = list(c = 3L, d = 4L))),
        ":: a\n\n1\n\n:: b\n\n2\n\n:: cd: c\n\n3\n\n:: cd: d\n\n4")
})


# flat_deserialize() -----------------------------------------------------------


test_that("_deserialize() returns a named list", {
    out <- flat_deserialize(flat_serialize(list(a = 1L, b = 2L, c = 3L)))

    expect_type(out, "list")
    expect_length(out, 3L)
    expect_named(out, c("a", "b", "c"))
})

test_that("_deserialize() validates string", {
    expect_error(flat_deserialize(1L))
    expect_snapshot(flat_deserialize(1L), error = TRUE)
})

test_that("_deserialize() validates tag_sep", {
    expect_error(flat_deserialize(tag_sep = 1L))
    expect_snapshot(flat_deserialize(tag_sep = 1L), error = TRUE)
})

test_that("_deserialize() deserializes empty character strings to empty lists", {
    expect_identical(flat_deserialize(""), list())
})

test_that("_deserialize() removes comments", {
    flat_1   <- ":: a\n\n1\n# Comment to ignore.\n\n:: b\n\n2\n\n:: c\n\n3"
    flat_2   <- ":: a\n\n1 # In-line comment to ignore.\n\n:: b\n\n2\n\n:: c\n\n3"
    expected <- list(a = "1", b = "2", c = "3")

    expect_identical(flat_deserialize(flat_1), expected)
    expect_identical(flat_deserialize(flat_2), expected)
})

test_that("_deserialize() treats escaped # as regular # being part of the contents", {
    # An escaped octothorpe (\#) is written \\# because \ must
    # be escaped in R. We also test that \\\# (written \\\\\\#)
    # yields a regular octothorpe for consistency.
    flat_1 <- ":: a\n\n\\# This is a valid octothorpe."
    flat_2 <- ":: a\n\n\\\\\\# This is a valid octothorpe."

    expect_identical(flat_deserialize(flat_1)$a, "# This is a valid octothorpe.")
    expect_identical(flat_deserialize(flat_2)$a, "# This is a valid octothorpe.")
})

test_that("_deserialize() handles mixed comments and escaped octothorpes", {
    flat     <- paste0(text_read(mock_flat_object_path), collapse = "\n")
    expected <- list(
        Remove = list(
            `1` = "Remove.",   # Normal comments are ignored.
            `2` = "Remove.",   # \\# is the same as #.
            `3` = "Remove."),  # \\\\# is the same as \\#.
        Keep = list(
            `1` = "# This is a valid octothorpe.\n\nKeep. # This is a valid octothorpe.",  # \# is a regular #.
            `2` = "# A comment to keep.\n\nKeep. # A comment to keep."))                   # \\\# is the same as \#.

    expect_identical(flat_deserialize(flat), expected)
})

test_that("_deserialize() handles multi-line contents appropriately", {
    flat <- ":: a\n\nThis is\nmulti-line\n\ncontents\n\n\nwith\nspaces.\n\n:: b\n\n2"

    expect_identical(
        flat_deserialize(flat),
        list(a = "This is\nmulti-line\n\ncontents\n\n\nwith\nspaces.", b = "2"))
})

test_that("_deserialize() deserializes <empty-list> constants to empty lists", {
    empty_list <- list(a = list())

    expect_identical(flat_deserialize(flat_serialize(empty_list)), empty_list)
})

test_that("_deserialize() throws an error if a node has another child node", {
    # This is obviously not possible.
    # A node is always terminal by design.
    expect_error(flat_deserialize(":: Level: Node\n\na\n\n:: Level: Node: Lower node\n\nb"))
    expect_snapshot(error = TRUE, {
        "This is an invalid FLAT object."
        invalid <- ":: Level: Node\n\na\n\n:: Level: Node: Lower node\n\nb"
        cat(invalid, "\n")

        flat_deserialize(invalid)
    })
})


# flat_tag() -------------------------------------------------------------------


test_that("_tag() returns a character", {
    expect_type(flat_tag(), "character")
    expect_length(flat_tag(), 0L)

    expect_type(flat_tag(list(1L)), "character")
    expect_length(flat_tag(list(1L)), 1L)
})

test_that("_tag() validates x", {
    expect_error(flat_tag(1L))
    expect_snapshot(flat_tag(1L), error = TRUE)
})

test_that("_tag() validates tag_sep", {
    expect_error(flat_tag(tag_sep = 1L))
    expect_snapshot(flat_tag(tag_sep = 1L), error = TRUE)
})

test_that("_tag() validates tag_empty", {
    expect_error(flat_tag(tag_empty = 1L))
    expect_snapshot(flat_tag(tag_empty = 1L), error = TRUE)
})

test_that("_tag() creates tags from names", {
    x <- list(a = 1L, b = 2L, cd = list(c = 3L, d = 4L), e = list())

    expect_identical(flat_tag(x), c("a", "b", "cd: c", "cd: d", "e: "))
})

test_that("_tag() replaces empty names by positions and tag_empty", {
    x <- list(a = 1L, b = 2L, list())

    expect_identical(flat_tag(x), c("a", "b", "[3]: "))
    expect_identical(flat_tag(x, tag_empty = "_"), c("a", "b", "_[3]: "))
})

test_that("_tag() retains the structure of x when replacing empty names by positions", {
    x <- list(1L, 2L, list(3L, 4L), 5L)

    expect_identical(flat_tag(x), c("[1]", "[2]", "[3]: [1]", "[3]: [2]", "[4]"))
})


# flat_format() ----------------------------------------------------------------


test_that("_format() returns a constant if x is empty", {
    expect_identical(flat_format(), "<empty list>")
})

test_that("_format() returns a list having the same shape as x", {
    # Lengths, types, and attributes (like names) must be preserved.
    x <- list(a = 1L, b = 2L, cd = list(c = 3L, d = 4L))

    expect_identical(
        flat_format(x),
        list(a = "1", b = "2", cd = list(c = "3", d = "4")))
})

test_that("_format() validates x", {
    expect_error(flat_format(1L))
    expect_snapshot(flat_format(1L), error = TRUE)
})

test_that("_format() coerces elements of x to character strings", {
    # We trust format() works as expected and only check
    # whether paste0() is called appropriately. We also
    # check this is done recursively.
    x <- list(
        a = logical(2L),
        b = integer(2L),
        c = double(2L),
        d = complex(2L),
        e = character(2L),
        f = raw(2L),
        g = list(
            a = logical(2L),
            b = integer(2L),
            c = double(2L),
            d = complex(2L),
            e = c("a", "b"),
            f = raw(2L)))

    expect_identical(
        flat_format(x),
        list(
            a = "FALSE\nFALSE",
            b = "0\n0",
            c = "0\n0",
            d = "0+0i\n0+0i",
            e = "\n",
            f = "00\n00",
            g = list(
                a = "FALSE\nFALSE",
                b = "0\n0",
                c = "0\n0",
                d = "0+0i\n0+0i",
                e = "a\nb",
                f = "00\n00")))
})

test_that("_format() replaces empty lists by a constant", {
    expect_identical(flat_format(list()), "<empty list>")
    expect_identical(flat_format(list(list())), list("<empty list>"))
    expect_identical(flat_format(list(list(list()))), list(list("<empty list>")))
})


# flat_example() ---------------------------------------------------------------


test_that("_example() returns a character string invisibly", {
    withr::local_output_sink(tempfile())
    out <- flat_example()

    expect_type(out, "character")
    expect_length(out, 1L)
    expect_invisible(flat_example())
})

test_that("_example() prints the example before returning", {
    expect_output(flat_example())
    expect_snapshot(flat_example())
})
