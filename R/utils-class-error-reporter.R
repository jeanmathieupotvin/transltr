ErrorReporter <- R6::R6Class("ErrorReporter",
    lock_class   = TRUE,
    lock_objects = TRUE,
    cloneable    = TRUE,
    private      = list(
        .errors      = list(),
        .error_label = NULL
    ),
    active = list(
        size = \(value) {
            if (!missing(value)) {
                stops("'$size' is a read-only value.")
            }

            return(length(private$.errors))
        },
        n_errors = \(value) {
            if (!missing(value)) {
                stops("'$n_errors' is a read-only value.")
            }

            # Using self$errors is crucial because it drops
            # NULL values before assessing length of errors.
            return(length(self$errors))
        },
        errors = \(value) {
            if (!missing(value)) {
                stops("'$errors' is a read-only value.")
            }

            # Drop NULLs and coerce remaining values
            # to a character. Order is preserved.
            return(unlist(private$.errors, TRUE, FALSE) %??% character())
        },
        error_label = \(value) {
            if (!missing(value)) {
                if (!is.null(value) && !is_chr1(value)) {
                    stops("'$error_label' must be 'NULL', or a non-NA and non-empty character of length 1.")
                }

                private$.error_label <- value
            }

            return(private$.error_label)
        }
    ),
    public = list(
        initialize = \(size = 25L) {
            assert_int1(size)
            private$.errors <- vector("list", size)
            return(self)
        },
        push = \(error = "") {
            assert_chr1(error, TRUE)

            if (!is.null(self$error_label)) {
                error <- sprintf("[%s] %s", self$error_label, error)
            }

            # If we reached the capacity of the
            # stack, grow it automatically.
            if (self$n_errors == self$size) {
                self$grow()
            }

            private$.errors[[self$n_errors + 1L]] <- error
            return(invisible(self))
        },
        pop = \() {
            if (self$n_errors > 0L) {
                # list(NULL) preserves the underlying slot.
                # Setting it equal to NULL would remove it
                # from private$.errors and the stack would
                # shrink by 1. We preserve size. Only
                # $grow_size() can increment it.
                private$.errors[[self$n_errors]] <- list(NULL)
            }

            return(invisible(self))
        },
        report = \(throw = FALSE) {
            assert_lgl1(throw)

            n_errors <- self$n_errors

            if (!n_errors) {
                return(character())
            }

            if (throw) {
                if (n_errors > 1L) {
                    stops(
                        "multiple errors encountered:\n",
                        paste0(" - ", self$errors, "\n"))
                }

                stops(self$errors)
            }

            return(self$errors)
        },
        as_list = function() {
            return(
                list(
                    size     = self$size,
                    n_errors = self$n_errors,
                    errors   = self$errors))
        },
        grow = \(size_add = 25L) {
            assert_int1(size_add)

            if (size_add > 0L) {
                private$.errors <- c(private$.errors, vector("list", size_add))
            }

            return(invisible(self))
        }
    )
)

error_reporter <- function(size = 25L) {
    return(ErrorReporter$new(size))
}

is_error_reporter <- function(x) {
    return(inherits(x, "ErrorReporter"))
}

#' @export
format.ErrorReporter <- function(x, indent = 1L, ...) {
    return(format_vector(x$as_list(), level = indent, indent = 1L))
}

#' @export
print.ErrorReporter <- function(x, ...) {
    cat("<ErrorReporter>", format(x, ...), sep = "\n")
    return(invisible(x))
}
