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

translator_scope <- function() {
    if (sys.nframe() < 2L) {
        return("global")
    }

    # Traverse the call stack and get the scope of
    # each called function. We use sys.function(i)
    # starting at i = 1 to do so, because 0 is the
    # current call to translator_get(). Here, i is
    # the number of generation to go back into the
    # stack. Using sys.parents() + 1L is slightly
    # faster than seq_len(sys.nframe()). The next
    # line of code below alters the stack, but in
    # a way that is safe (top to bottom) for what
    # we are trying to achieve.
    scopes <- vapply_1c(sys.parents() + 1L, \(i) {
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
