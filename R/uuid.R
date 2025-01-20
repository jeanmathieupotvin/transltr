#' Universally Unique Identifiers
#'
#' Generate a random UUID (*Universally Unique Identifier*) that complies to
#' what [RFC4122](https://www.rfc-editor.org/rfc/rfc4122) prescribes. Such a
#' value is also known as a *version 4* UUID.
#'
#' [uuid()] calls [uuid_raw()] and formats its output accordingly.
#'
#' Pseudo-random bytes are generated with [sample()] whenever [uuid_raw()]
#' is called. This is most likely done before runtime when
#' [`Translator`][Translator] objects are created. [uuid_raw()] samples values
#' in the `[0, 255]` range with replacement and converts them to [raw][raw()]
#' values. The user must ensure that the underlying seed is appropriate when
#' generating UUIDs. See [set.seed()] for more information.
#'
#' @param x An \R object.
#'
#' @returns
#' [uuid()] returns a character of length 1 containing exactly 36
#' characters: 32 hexadecimal characters and 4 hyphens (used as separators).
#'
#' [uuid_raw()] returns a raw vector of length 16.
#'
#' [uuid_is()] returns a logical vector having the same length as `x`. It
#' checks whether its elements are valid version 4 (variant 1) UUIDs or
#' not. It returns `FALSE` for any other kind of UUID.
#'
#' @note
#' UUIDs are designed to be globally unique (collisions are extremely unlikely)
#' and are sometimes called GUIDs (*Globally Unique Identifiers*). There are
#' several UUID versions with slightly different purposes.
#'
#' Package [`transltr`][transltr] uses random identifiers (version 4/variant 1,
#' also known as DCE 1.1, ISO/IEC 11578:1996).
#'
#' @seealso
#' [RFC4122](https://www.rfc-editor.org/rfc/rfc4122),
#' [UUIDs explained](https://www.uuidtools.com/uuid-versions-explained)
#'
#' @examples
#' uuid()
#' uuid_raw()
#' uuid_is(uuid())      ## TRUE
#' uuid_is(uuid_raw())  ## FALSE, uuid_raw() does not return a string.
#'
#' @keywords internal
#' @export
uuid <- function() {
    chars <- as.character(uuid_raw())
    storage.mode(chars) <- "list"
    return(do.call(sprintf, c("%s%s%s%s-%s%s-%s%s-%s%s-%s%s%s%s%s%s", chars)))
}

#' @rdname uuid
#' @keywords internal
#' @export
uuid_raw <- function() {
    # Useful reminder for standard UUID structure.
    #
    # 0 - 3      4 - 5      6 - 7                 8                          9               10 - 15
    # 32 bits    16 bits    16 bits               8 bits                     8 bits          48 bits
    # time_low - time_mid - time_hi_and_version - clock_seq_hi_and_reserved  clock_seq_low - node
    # xxxxxxxx - xxxx     - 4xxx                - [02-8a-f]x                 xx            - xxxxxxxxxxxxxxxx
    #                       ^                        ^
    #                       |                        |
    #                       Algorithm version (4)    Algorithm variant (none)
    #
    # Source: https://www.rfc-editor.org/rfc/rfc4122
    # Source: https://www.uuidtools.com/uuid-versions-explained

    # Generate 16 pseudo-random bytes. They are not cryptographically
    # secure because this is not required for simple identitication
    # purposes. We just require a low collision risks.
    bytes <- as.raw(sample(seq.int(0L, 255L), 16L, replace = TRUE))

    # Set version equal to 4 [RFC4122/section 4.4].
    # To do so, first two bits of byte 7 must be set to (01).
    # This is not obvious at first glance. Use this example to understand.
    #
    # a       8        # input byte (example for 0xa8)
    # 1 0 1 0 1 0 0 0  # input bits (most signif. bit first)
    # & & & & & & & &
    # 0 0 0 0 1 1 1 1  # 0x0f
    # ---------------
    # 0 0 0 0 1 0 0 0  # temporary bits
    # | | | | | | | |
    # 0 1 0 0 0 0 0 0  # 0x40
    # ---------------
    # 0 1 0 0 1 0 0 0  # final bits
    # 4       x        # final byte in hex notation
    #
    # Source: https://github.com/uuidjs/uuid/blob/main/src/v4.js
    bytes[7L] <- (bytes[7L] & as.raw(0x0f)) | as.raw(0x40)

    # Set variant equal to default one [RFC4122/section 4.4].
    # To do so, first two bits of byte 9 must be set to (10).
    # Strategy is identical to above. Use this example to understand.
    #
    # 2       b        # input byte (example for 0x2b)
    # 0 0 1 0 1 0 1 1  # input bits (most signif. bit first)
    # & & & & & & & &
    # 0 0 1 1 1 1 1 1  # 0x3f
    # ---------------
    # 0 0 1 0 1 0 1 1  # temporary bits
    # | | | | | | | |
    # 1 0 0 0 0 0 0 0  # 0x80
    # ---------------
    # 1 0 1 0 1 0 1 1  # final bits
    # a       x        # final byte
    #
    # Source: https://github.com/uuidjs/uuid/blob/main/src/v4.js
    bytes[9L] <- (bytes[9L] & as.raw(0x3f)) | as.raw(0x80)

    return(bytes)
}

#' @rdname uuid
#' @keywords internal
#' @export
uuid_is <- function(x) {
    if (!is_chr(x, FALSE)) {
        return(FALSE)
    }

    return(grepl("^[0-9a-fA-F)]{8}-[0-9a-f|A-F]{4}-4[0-9a-fA-F]{3}-[89ab][0-9a-fA-F)]{3}-[0-9a-fA-F]{12}$", tolower(x)))
}
