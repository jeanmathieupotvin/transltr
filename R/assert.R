#' Assert that a value is valid
#'
#' Validate a value and throw an error if it is not.
#'
#' @param x An \R object to be tested.
#'
#' @inheritParams assert-backend
#'
#' @returns
#' Argument `x` is returned invisibly if valid.
#'
#' @name assert
#'
#' @author Jean-Mathieu Potvin (<jm@@potvin.xyz>)
#'
#' @examples
#' myValue <- 1
#'
#' \dontrun{
#' assertString(myValue)
#' # Error: TypeError: `myValue` must be a single non-NA character value.
#'
#' assertString(myValue, "otherName")
#' # Error: TypeError: `otherName` must be a single non-NA character value.
#' }
#'
#' @family assertions
#'
#' @keywords internal
assertString <- function(x, name = deparse(substitute(x))) {
    .assertScalar(x, FALSE, "character", is.character, name)

    # A string is considered to be a
    # subtype of a scalar character.
    if (!nzchar(x)) {
        stopf(
            "TypeError",
            "`%s` must be a single non-NA, non-empty, character value.",
            name)
    }

    return(invisible(x))
}

#' @rdname assert
#' @keywords internal
assertExistingFile <- function(x, name = deparse(substitute(x))) {
    assertString(x, name)

    if (!utils::file_test("-f", x)) {
        stopf("InterfaceError", "`%s` must be an existing file.", name)
    }

    return(invisible(x))
}


# Low-level workhorse functions ------------------------------------------------


#' Create assertions
#'
#' For advanced use only. Functions described here are building blocks that
#' can be used to create further higher-order assertions.
#'
#' Because [.assertScalar()], [.assertNonEmpty()], and [.assertN()] are meant
#' to be embedded into other functions by the developers, their arguments are
#' **not validated** for efficiency. They should be used to create other
#' `assert*()` functions that are semantic, more specific, and/or more complex.
#'
#' They differ only by the length they expect.
#'
#' * [.assertN()] enforces a specific length, `n`, which can be 0,
#' * [.assertNonEmpty()] enforces any non-zero length, and
#' * [.assertScalar()] enforces a length equal to `1` (scalar value).
#'
#' @name assert-backend
#'
#' @param x An \R object to be tested.
#'
#' @param n An integer value greater than or equal 0. The expected length
#'   of `x`.
#'
#' @param na A logical value. Can `x` contain [NA][base::NA] value(s)?
#'
#' @param type A character string. The name of the expected
#'   [type][base::typeof()], [class][base::class()], or shape of `x`. It is
#'   used to construct meaningful error messages if `x` is not of the given
#'   `type`.
#'
#' @param is An introspector function that returns a single logical value.
#'   It is used to assess whether `x` is of the given `type`. It must accept
#'   a single argument.
#'
#'   ```
#'   function(x) { return(logical(1L)) }
#'   ```
#'
#' @param name A character string. A value representing `x` that is used to
#'   construct meaningful error messages. By default, this corresponds to the
#'   underlying [symbol][base::name] passed to `x`.
#'
#' @returns
#' Argument `x` is returned invisibly if valid.
#'
#' @author Jean-Mathieu Potvin (<jm@@potvin.xyz>)
#'
#' @family assertions
#'
#' @keywords internal
.assertN <- function(
    x,
    n    = integer(1L),
    na   = FALSE,
    type = character(1L),
    is   = is(),
    name = deparse(substitute(x)))
{
    if (!is(x) || length(x) != n || (!na && anyNA(x))) {
        msg <- if (na) {
            "`%s` must be a %s vector of %i elements."
        } else {
            "`%s` must be a %s vector of %i non-NA elements."
        }

        stopf("TypeError", msg, name, type)
    }

    return(invisible(x))
}

#' @rdname assert-backend
#' @keywords internal
.assertNonEmpty <- function(
    x,
    na   = FALSE,
    type = character(1L),
    is   = is(),
    name = deparse(substitute(x)))
{
    if (!is(x) || !length(x) || (!na && anyNA(x))) {
        msg <- if (na) {
            "`%s` must be a non-empty %s vector."
        } else {
            "`%s` must be a non-empty %s vector of non-NA elements."
        }

        stopf("TypeError", msg, name, type)
    }

    return(invisible(x))
}

#' @rdname assert-backend
#' @keywords internal
.assertScalar <- function(
    x,
    na   = FALSE,
    type = character(1L),
    is   = is(),
    name = deparse(substitute(x)))
{
    if (!is(x) || length(x) != 1L || (!na && anyNA(x))) {
        msg <- if (na) {
            "`%s` must be a single %s value."
        } else {
            "`%s` must be a single non-NA %s value."
        }

        stopf("TypeError", msg, name, type)
    }

    return(invisible(x))
}
