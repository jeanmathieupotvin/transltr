# FIXME: add documentation on the limits of using translator_set().

#' @export
translator_set <- function(x = translator(), scope = NULL) {
    scope <- scope %??% translator_scope()
    assert_chr1(scope)

    if (is.null(x)) {
        suppressWarnings(rm(list = scope, pos = .__translators_cache__))
        return(invisible())
    }
    if (!is_translator(x)) {
        stops("'x' must be a 'Translator' object.")
    }

    assign(scope, x, .__translators_cache__)
    return(invisible())
}

#' @export
translator_get <- function(scope = NULL) {
    scope <- scope %??% translator_scope()
    assert_chr1(scope)
    return(.__translators_cache__[[scope]])
}

#' @rdname translator-scope
#' @keywords internal
translator_scope <- function() {
    if (sys.nframe() < 2L) {
        # This must be tested manually by calling
        # the function from a global environment.
        # We cannot control the call stack while
        # using testthat, because it is in charge
        # of executing code. There is also no way
        # to alter the call stack.
        return("global") # nocov
    }

    # Traverse the call stack and get the scope of
    # each called function. We use sys.function(i)
    # starting at i = 0 (the current call). Doing
    # so alters the call stack, by adding further
    # calls to it. Specifically, 3 calls are added,
    # and we take them into account to access the
    # call stack's state prior to vapply_1c().
    # Also, index of sys.function() starts at 0.
    scopes <- vapply_1c(-seq_len(sys.nframe()) + 1L - 3L, \(i) {
        return(translator_scope_name(sys.function(i)))
    })

    # Some namespaces are excluded because they are
    # not relevant. Currently, we exclude transltr
    # and base. The former does not concern users,
    # and the latter will never use transltr. This
    # allows embedding translate() calls in other
    # usual base functions. In the future, transltr
    # may still use this function as is to translate
    # itself, because which.max() will return 1 (and
    # match scope "transltr" in scopes).
    return(scopes[[which.max(!match(scopes, .__CHR_EXCLUDED_NS, 0L))]])
}

#' @rdname translator-scope
#' @keywords internal
translator_scope_name <- function(x) {
    scope <- switch(class(x),
        character  = x,
        `function` = {
            name <- environmentName(environment(x))
            if (nzchar(name)) name else "global"
        },
        environment = {
            name <- environmentName(x)
            if (nzchar(name)) name else "global"
        },
        "global")

    if (scope == "R_GlobalEnv") {
        return("global")
    }

    return(if (nzchar(scope)) scope else "global")
}

# This is an internal cache into which Translator objects
# are registered via translator_set(). Translator objects
# are R6 instances, and these are just fancy environments.
# Environments have by-reference semantics in R. Therefore,
# even if a Translator has multiple bindings (one for the
# user and one in the cache, typically), changes can be
# made without re-setting objects over and over again.
.__translators_cache__ <- new.env(parent = emptyenv())
