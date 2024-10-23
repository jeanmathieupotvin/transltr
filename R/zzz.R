#' Package hooks
#'
#' These hooks are called just after loading and unloading the namespace of
#' [`transltr`][transltr]. See [getHook()] for details.
#'
#' [.hook_load()] and [.hook_unload()] are internally wrapped by \R's standard
#' hook functions [.onLoad()] and [.onUnload()] (as usual). The former are kept
#' separate for documentation and testing purposes.
#'
#' @returns
#' A `NULL` invisibly. Hooks are used for their side-effect.
#'
#'   * [.hook_load()] creates an empty environment named
#'     [`.__STR_ATTACHED_DB`][.__STR_ATTACHED_DB] and attaches it to the
#'     [search path][search()].
#'   * [.hook_unload()] detaches and removes it.
#'
#' @note
#' Calling [.hook_unload()] manually will break core mechanisms of the package.
#'
#' @rdname hooks
#' @keywords internal
.hook_load <- function() {
    # The following conditional statements cover
    # unusual cases where users might use devtools
    # and/or pkgload, or similar alternatives to
    # library() and friends. It prevents transltr
    # from attaching multiple environments having
    # the same name to the search path.
    if (is_match(.__STR_ATTACHED_DB, search())) {
        return(invisible())
    }

    attach(NULL, name = .__STR_ATTACHED_DB)
    return(invisible())
}

#' @rdname hooks
#' @keywords internal
.hook_unload <- function() {
    detach(.__STR_ATTACHED_DB, character.only = TRUE)
    return(invisible())
}

# Register hooks via R standard interface.
.onLoad   <- function(libname, pkgname) .hook_load() # nocov
.onUnload <- function(libpath) .hook_unload() # nocov
