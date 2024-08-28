translations_env <- function(header = translations_header(), blocks = list()) {
    return(TranslationsEnv$new(header, blocks))
}

is_translation_env <- function(x) {
    return(inherits(x, "TranslationsEnv"))
}

TranslationsEnv <- R6::R6Class("TranslationsEnv",
    lock_class = FALSE,
    private = list(
        env = NULL
    ),
    public = list(
        header = NULL,
        initialize = function(header = translations_header(), blocks = list()) {
            if (!is_translation_header(header)) {
                stops("'header' must be a 'TranslationHeader' object.")
            }
            if (!all(vapply_1l(blocks, is_translations_block))) {
                stops("'blocks' must be a 'TranslationsBlock' object.")
            }

            names(blocks) <- vapply_1c(blocks, `[[`, i = "id")
            private$env   <- list2env(blocks)
            self$header   <- header

            lockEnvironment(private$env, TRUE)
            return(self$validate())
        },
        validate = function() {
            ids    <- names(private$env)
            hashes <- self$header$.hashes

            # Validate that header's hashes all
            # match TranslationBlock identifiers.
            if (length(ids) != length(hashes)) {
                stops(
                    "the number of hashes in header's field '.hashes' must ",
                    "be equal to the number of 'TranslationBlock' objects.")
            }
            if (anyDuplicated(hashes)) {
                stops("all hashes in header's field '.hashes' must be unique.")
            }
            if (anyDuplicated(ids)) {
                stops("all 'TranslationBlock' identifiers ('id') must be unique.")
            }
            if (anyNA(pmatch(ids, hashes))) {
                stops(
                    "at least one 'TranslationBlock' identifier`('id') ",
                    "does not match a hash in header's field '.hashes'.")
            }

            return(invisible(self))
        },
        get = function(hash = "", lang = "") {
            assert_chr1(hash)
            assert_chr1(lang)

            if (is.null(block <- private$env[[hash]])) {
                stopf("'%s' is not a registered hash.", hash)
            }

            return(block[["translations"]][[lang]][["text"]])
        }
    )
)
