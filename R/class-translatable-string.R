#' Translatable strings
#'
#' @description
#' Mark strings as being translatable, sanitize, and hash them.
#'
#' @returns
#' [as.TranslatableString()] and related methods return a
#' [TranslatableString] object.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @seealso [translate()],
#'   [as.TranslatableString.TranslateCall()]
#'
#' @rdname class-translatable-string
#'
#' @importFrom R6 R6Class
#' @importFrom sodium bin2hex
#' @importFrom sodium sha256
#'
#' @keywords internal
TranslatableString <- R6::R6Class(
    classname    = "TranslatableString",
    inherit      = NULL,
    class        = TRUE,
    portable     = TRUE,
    lock_objects = TRUE,
    lock_class   = TRUE,
    cloneable    = FALSE,
    active       = list(
        #' @field string a sanitized character of length 1.
        string = function() {
            return(private$.string)
        },

        #' @field signature a character of length 1. The
        #'   [SHA-256](https://en.wikipedia.org/wiki/SHA-2)
        #'   hash of field `$string`.
        signature = function() {
            return(sodium::bin2hex(sodium::sha256(charToRaw(private$.string))))
        },

        #' @field tokens values previously passed to method
        #'   [`$new()`][TranslatableString] and coerced to a character.
        tokens = function() {
            return(private$.tokens)
        }
    ),
    public = list(
        #' @description
        #' Create a [TranslatableString] object.
        #'
        #' @param ... values to be concatenated into a single character string.
        #'
        #' @param concat a character string used to concatenate values passed
        #'   to `...`
        #'
        #' @return
        #' A [R6][R6::R6] object of class [TranslatableString].
        initialize = function(..., concat = " ") {
            if (!isSingleChar(concat)) {
                stopf(
                    "`concat` must be a single, non-empty,",
                    "and non-NA character of any size. See `?nchar()`.")
            }

            # unlist(list()) better handles nested objects than c().
            if (is.list(tokens <- unlist(list(...), TRUE, FALSE))) {
                stopf("all values must be coercible to a character vector.")
            }

            private$.tokens <- tokens
            private$.string <- private$.sanitize(tokens, concat)
            return(self)
        },

        #' @description
        #' Format this object.
        #'
        #' @param signatureLength an integer between `0` and `64`. Desired
        #'   length of `$signature`.
        #'
        #'   * Leave it at `64` to use its full value.
        #'   * Pass `0` to omit it from `header`.
        #'   * Pass any other valid value to use its `signatureLength` first
        #'     characters.
        #'
        #' @return
        #' A named character vector of length 2 containing the
        #' following elements:
        #'
        #' \describe{
        #'   \item{`header`}{a character string containing the
        #'     `signatureLength` first characters of field `$signature`.}
        #'   \item{`string`}{current value of field `$string`.}
        #' }
        format = function(signatureLength = 64L) {
            if (!isSingleIntegerInRange(signatureLength, 0L, 64L)) {
                stopf(
                    "`signatureLength` must be a single integer value",
                    "greater than or equal to 0L (no `$signature`) and",
                    "lesser than or equal to 64L (full `$signature`).")
            }

            subClass     <- class(self)[[1L]]
            subSignature <- substr(self$signature, 1L, signatureLength)

            header <- if (signatureLength) {
                sprintf("<%s: %s>", subClass, subSignature)
            } else {
                sprintf("<%s>", subClass)
            }

            return(c(header = header, string = self$string))
        },

        #' @description
        #' Print this object.
        #'
        #' @param signatureLength passed to method [`$format()`][TranslatableString].
        #'
        #' @return
        #' This object invisibly.
        print = function(signatureLength = 64L) {
            cat(self$format(signatureLength)[["header"]], sep = "\n")
            return(self$cat())
        },

        #' @description
        #' Print unquoted and formatted value of field `$string`.
        #'
        #' @details
        #' Nothing is printed if `$string` is empty.
        #'
        #' @return
        #' This object invisibly.
        cat = function() {
            if (nzchar(string <- self$string)) {
                cat(strwrap(string, 60L, 1L), sep = "\n ")
            }

            return(invisible(self))
        },

        #' @description
        #' Coerce this object to a character.
        #'
        #' @return
        #' A single character. Current value of field `$string`. It has
        #' an attribute named `signature` corresponding to field `$signature`.
        asCharacter = function() {
            return(structure(self$string, signature = self$signature))
        },

        #' @description
        #' Coerce this object to a list.
        #'
        #' @return
        #' A named list. Each element is a field of this object.
        #' See section **Active bindings** above.
        asList = function() {
            return(
                list(
                    signature = self$signature,
                    string    = self$string,
                    tokens    = self$tokens))
        }
    ),
    private = list(
        .tokens = character(),
        .string = character(1L),

        # @description
        # Concatenate multiple values into a single
        # character string and sanitize them.
        #
        # @param tokens a character. Values to be combined into a single
        #   character string.
        #
        # @param concat a character string used to concatenate `tokens`.
        #
        # @return
        # A character string.
        .sanitize = function(tokens = character(), concat = " ") {
            string <- paste0(tokens, collapse = concat)
            return(gsub("[\n\t\r ]+", " ", string))
        }
    )
)

#' @rdname class-translatable-string
#'
#' @param x an \R object.
#'
#' @param ... further arguments passed to or from other methods.
#'
#' @export
as.list.TranslatableString <- function(x, ...) {
    return(x$asList())
}

#' @rdname class-translatable-string
#' @export
as.character.TranslatableString <- function(x, ...) {
    return(x$asCharacter())
}

#' @rdname class-translatable-string
as.TranslatableString <- function(x, ...) {
    UseMethod("as.TranslatableString")
}
