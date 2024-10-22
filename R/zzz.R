.onLoad <- function(libname, pkgname) {
    if (is_match(.__STR_ATTACHED_DB, search())) {
        return(invisible())
    }

    attach(NULL, name = .__STR_ATTACHED_DB)
    return(invisible())
}

.onUnload <- function(libpath) {
    detach(.__STR_ATTACHED_DB, character.only = TRUE)
    return(invisible())
}
