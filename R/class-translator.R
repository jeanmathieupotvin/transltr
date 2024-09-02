Translator <- R6::R6Class("Translator",
    lock_class = FALSE,
    private = list(
        env = NULL
    ),
    public = list(
        file_path      = "",
        hash_algorithm = "blake2b",
        hash_length    = 32L,
        hashes         = character(),
        language_keys  = character(),
        further_fields = list(),
        initialize = function(
            blocks         = list(),
            file_path      = "",
            hash_algorithm = "blake2b",
            hash_length    = 32L,
            hashes         = character(),
            language_keys  = character(),
            ...)
        {
            if (!all(vapply_1l(blocks, is_block))) {
                stops("'blocks' must only contain 'Block' object.")
            }

            self$file_path      <- file_path
            self$hash_algorithm <- hash_algorithm
            self$hash_length    <- hash_length
            self$hashes         <- hashes
            self$language_keys  <- language_keys
            self$further_fields <- list(...)

            names(blocks) <- vapply_1c(blocks, `[[`, i = "id")
            private$env   <- list2env(blocks)

            lockEnvironment(private$env, TRUE)
            return(self$validate())
        },
        validate = function() {
            assert_chr1(file_path, TRUE)
            assert_arg(hash_algorithm, TRUE)
            assert_int1(hash_length)
            assert_chr(hashes, TRUE)
            assert_chr(language_keys, TRUE)
            assert_names(language_keys)

            ids    <- names(private$env)
            hashes <- self$hashes

            if (!is_named(further_fields)) {
                stops("all further fields must be named.")
            }
            if (length(hashes) && any(nchar(hashes) != hash_length)) {
                stops("all 'hashes' must have a length equal to 'hash_length'.")
            }

            # Registered hashes must match exactly 1 Block identifier.
            if (length(ids) != length(hashes)) {
                stops(
                    "the number of hashes in 'hashes' must ",
                    "be equal to the number of 'Block' objects.")
            }
            if (anyDuplicated(hashes)) {
                stops("all hashes in 'hashes' must be unique.")
            }
            if (anyDuplicated(ids)) {
                stops("all 'Block' identifiers ('id') must be unique.")
            }
            if (anyNA(pmatch(ids, hashes))) {
                stops(
                    "at least one 'Block' identifier ",
                    "does not match a hash in 'hashes'.")
            }

            return(invisible(self))
        },
        # print = function() {
        #     keys <- self$header$language_keys
        #     keys_by_id <- eapply(private$env, \(block) {
        #         return(
        #             to_string(
        #                 names(block[["translations"]]),
        #                 last_sep = ", "))
        #     })

        #     cat("<TranslationEnv>\n",
        #         " Registered keys:\n",
        #         sprintf("   [ %s ]: %s\n", names(keys), keys),
        #         " Registered identifiers:\n",
        #         sprintf("   [ %s ]: %s\n", names(keys_by_id), keys_by_id),
        #         sep = "")

        #     return(invisible(self))
        # },
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

translator <- Translator$new

is_translator <- function(x) {
    return(inherits(x, "Translator"))
}
