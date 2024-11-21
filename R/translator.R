#' Get or Set Translator Objects
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' **These features are experimental. Use with caution.**
#'
#' Get or set the [`Translator`][Translator] object to use when calling
#' [translate()]. It is registered within an internal cache managed by
#' [`transltr`][transltr].
#'
#' @details
#' Translations performed by [translate()] always have a `scope` specifying
#' which [`Translator`][Translator] object must be used. This scope can be
#' explicit, or implicit.
#'
#' ## Explicit Scopes
#'
#' Explicit scopes are entirely determined, and managed by the user. In that
#' case, argument `scope` of [translator_set()], [translator_get()], and
#' [translate()] must be set to the appropriate value. **Scopes are shared**,
#' and **can be overwritten**. When choosing to use explicit scopes, it is the
#' responsability of the user to ensure its scope(s) are collision-resistant.
#' It may use [translator_scopes()] to get a list of active scopes.
#'
#' ## Implicit Scopes
#'
#' Implicit scopes are the default. These are scopes entirely determined, and
#' managed by [`transltr`][transltr] (unless a user *unsets* them). They are
#' inferred from the underlying [namespace][getNamespace()] of the function
#' that (directly) calls either [translator_set()], [translator_get()], or
#' [translate()]. If it has no namespace, the `global` scope is used. There is
#' one exception: package `base` is always ignored and replaced by the global
#' scope. In other words, the scope will be `global`, unless [translator_set()],
#' [translator_get()], or [translate()] is directly called within a function
#' defined in any non-`base` package.
#'
#' Implicit scopes are determined at runtime by inspecting the call stack. \R
#' call stacks can be surprisingly complex, and in some non-standard situations,
#' [`transltr`][transltr] may fail to infer a scope. Future iterations will
#' bring more robust scoping mechanisms.
#'
#' @section Reference semantics:
#' [`Translator`][Translator] objects are [`R6`][R6::R6Class()] objects, and
#' [`R6`][R6::R6Class()] are stored as [environments][environment()]. In \R,
#' environments have reference semantics (they are not copied when changed).
#' Therefore, a [`Translator`][Translator] object only need to be set once,
#' and may be modified afterwards without having to call [translator_set()]
#' again.
#'
#' Moreover, removing a binding that references a [`Translator`][Translator]
#' object does not remove it from the internal cache. Users further have to
#' call [translator_set()] to do so.
#'
#' @param x A [`Translator`][Translator] object, or `NULL`. The latter is
#'   used to unset a previously registered [`Translator`][Translator] object.
#'
#' @template param-scope
#'
#' @returns
#' [translator_set()] returns `NULL`, invisibly. It is used for its side-effect
#' of registering `x`, and assigning it to a `scope`.
#'
#' [translator_get()] returns the [`Translator`][Translator] object assigned
#' to `scope`, or `NULL` if there is none.
#'
#' [translator_scopes()] returns a character vector.
#'
#' @examples
#' ## Setting and getting a Translator object having a 'global' scope.
#' translator_set(x = translator(id = "my-global-translator"))
#' translator_get()
#' translator_get("global")
#'
#' ## Unregister a Translator object (for implicit and explicit scopes).
#' translator_set(NULL)
#' translator_set(NULL, "global")
#' is.null(translator_get(NULL)) ## TRUE
#'
#' ## Setting and getting a Translator object, while letting them infer
#' ## the underlying scope. This is done temporarily within the utils
#' ## package for illustration purposes.
#' evalq(envir = asNamespace("utils"), \() {
#'   on.exit(translator_set(NULL))
#'   translator_set(translator(id = sprintf("utils:%s", transltr::uuid())))
#'   return(translator_get())
#' })()
#'
#' @seealso [translate()]
#'
#' @rdname translator-accessors
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

#' @rdname translator-accessors
#' @export
translator_get <- function(scope = NULL) {
    scope <- scope %??% translator_scope()
    assert_chr1(scope)
    return(.__translators_cache__[[scope]])
}

#' @rdname translator-accessors
#' @export
translator_scopes <- function() {
    return(sort(names(.__translators_cache__)))
}


#' Determine Scopes
#'
#' These functions work together to determine the underlying scope of
#' a call to [translator_set()], [translator_get()], or [translate()].
#'
#' [translator_scope_name()] constructs scope names from \R objects.
#'
#'   * It returns `x` as is if it is a character string.
#'   * It returns the [name][environmentName()] of `x` if it is an environment.
#'   * It returns the [name][environmentName()] of the enclosure of `x` if it
#'     is a [function].
#'   * It returns `"global"` for any other value passed to `x`.
#'
#' Empty outputs are always replaced by the default scope (`global`).
#'
#' [translator_scope()] is used to infer scopes by inspecting the call stack.
#' It is called by [translator_set()], [translator_get()], and [translate()].
#' \R call stacks can be surprisingly complex, and in some non-standard
#' situations, [`transltr`][transltr] may fail to infer a scope.
#'
#' @note
#' Developers should absolutely read code comments of these functions before
#' iterating on them.
#'
#' @param x An \R object.
#'
#' @returns A character string.
#'
#' @seealso [sys.nframe()],
#'   [sys.function()],
#'   [translator_set()],
#'   [translator_get()],
#'   [translate()]
#'
#' @rdname translator-scope
#' @keywords internal
translator_scope <- function() {
    # Traverse the call stack and get the scope of each
    # called function. We use sys.function(i) starting
    # at i = -3. This is because the traversal alters
    # the call stack's current state by adding further
    # calls to it. Specifically, 3 calls are added when
    # calling sys.function() in vapply_1c() below. Also,
    # index of sys.function() starts at 0, and calling
    # sys.function(-nframe) always returns NULL, which
    # is used to always add 'global' scope to the vector
    # of candidates.
    nframe <- sys.nframe()
    scopes <- vapply_1c(-seq.int(3L, nframe + 3L, 1L), \(i) {
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


#' Read and Write Portable Translators
#'
#' Read [Portable Translator Files] and parse them as [`Translator`][Translator]
#' objects, or convert [`Translator`][Translator] objects back to
#' [Portable Translators Files].
#'
#' @template param-path
#'
#' @template param-encoding
#'
#' @param x An object of class [`Translator`][Translator].
#'
#' @returns
#' [translator_import()] returns a [`Translator`][Translator] object if `path`
#' points to a valid translations source file.
#'
#' [translator_export()] is not yet implemented and throws an error.
#'
#' @seealso [Portable Translators Files]
#'
#' @rdname translator-io
#' @export
translator_import <- function(path = "", encoding = "UTF-8") {
    return(.NotYetImplemented())
}

#' @rdname translator-io
#' @export
translator_export <- function(x, path = "") {
    return(.NotYetImplemented())
}


# Internals --------------------------------------------------------------------


# This is an internal cache into which Translator objects
# are registered via translator_set(). Translator objects
# are R6 instances, and these are just fancy environments.
# Environments have reference semantics in R. Therefore,
# even if a Translator has multiple bindings (one for the
# user and one in the cache, typically), changes can be
# made without re-setting objects over and over again.
.__translators_cache__ <- new.env(parent = emptyenv())
