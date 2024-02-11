halt <- function(str = "", ..., caller = getCallerName(-2L)) {
    # Include name of caller function, if any.
    # From the perspective of getCallerName(), we
    # must go back 2 frames above in the stack:
    #  - caller()        (level -2)
    #  - halt()          (level -1)
    #  - getCallerName() (level  0).

    prefix <- if (!is.null(caller)) {
        if (!isNonEmptyString(caller)) {
            halt("'caller' must be a non-NA and non-empty character of length 1 or NULL.")
        }

        sprintf("<%s()>: ", caller)
    } else {
        ""
    }

    stop(ConditionMessage(str, ..., prefix = prefix), call. = FALSE)
}

ConditionMessage <- function(str = "", ..., prefix = "") {
    assertString(str)
    assertString(prefix)
    return(sprintf(sprintf("%s%s", prefix, str), ...))
}

getCallerName <- function(which = -1L) {
    if (-which > sys.nframe() || is.null(parentCall <- sys.call(which))) {
        return(NULL)
    }

    return(deparse1(parentCall[[1L]]))
}
