#' @export
translator_set <- function(x = translator()) {
    binding <- translator_binding(sys.function(-1L))

    if (is.null(x)) {
        suppressWarnings(base::rm(list = binding, pos  = .__STR_ATTACHED_DB))
        return(invisible())
    }
    if (!is_translator(x)) {
        stops("'x' must be 'NULL' or a 'Translator' object.")
    }

    assign(binding, x, .__STR_ATTACHED_DB, inherits = FALSE)
    return(invisible())
}

#' @export
translator_get <- function() {
    x <- if (sys.nframe() > 2L) sys.function(-2L) else NULL
    return(
        get0(
            translator_binding(x),
            mode       = "environment",
            inherits   = TRUE,
            ifnotfound = NULL))
}

translator_binding <- function(x = NULL) {
    name <- switch(typeof(x),
        `NULL`    = "R_GlobalEnv",
        character = x,
        closure   = {
            env_name <- environmentName(environment(x))
            if (nzchar(env_name)) env_name else "R_GlobalEnv"
        },
        stops("'x' must be a character string, a closure, or 'NULL'."))

    return(sprintf(".__transltr:translator:%s__", name))
}
