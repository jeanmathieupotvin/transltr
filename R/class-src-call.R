extract <- function(path = character(1L)) {
    if (!isNonEmptyString(path)) {
        stop(
            "'path' must be a non-NA and non-empty character of length 1.",
            call. = FALSE)
    }
    if (!utils::file_test("-f", path <- normalizePath(path, "/", FALSE))) {
        stop(
            sprintf("'%s' either does not exist or is not a valid file.", path),
            call. = FALSE)
    }

    srcExprs   <- .parseSrcExprs(path)
    toExtract  <- vapply(srcExprs, .isTranslateCall, NA)
    srcStrings <- lapply(srcExprs[toExtract], .as.SrcExprIntlString)

    attr(srcStrings, "path")      <- path
    attr(srcStrings, "basename")  <- basename(path)
    attr(srcStrings, "dirname")   <- dirname(path)
    attr(srcStrings, "timestamp") <- file.mtime(path)
    return(srcStrings)
}

.parseSrcExprs <- function(path = character(1L)) {
    tokens <- path |>
        parse(keep.source = TRUE) |>
        utils::getParseData(includeText = TRUE)

    exprs <- tokens[tokens$token == "expr", ]

    return(
        .mapply(.newSrcExpr, MoreArgs = list(), dots = list(
            text  = exprs$text,
            line1 = exprs$line1,
            line2 = exprs$line2,
            col1  = exprs$col1,
            col2  = exprs$col2)))
}

.newSrcExpr <- function(
    text  = character(1L),
    line1 = integer(1L),
    col1  = integer(1L),
    line2 = integer(1L),
    col2  = integer(1L))
{
    .newSrcExprCond <- function(cond) {
        msg <- cond$message
        class(msg) <- c("SrcExprCond", "list")
        return(msg)
    }

    token <- list(
        start  = c(line1, col1),
        end    = c(line2, col2),
        parsed = NULL)

    if (nzchar(text)) {
        token$parsed <- tryCatch(
            str2lang(text),
            error   = .newSrcExprCond,
            warning = .newSrcExprCond)
    }

    class(token) <- c("SrcExpr", "list")
    return(token)
}

.as.SrcExprIntlString <- function(x, ...) {
    UseMethod(".as.SrcExprIntlString")
}

#' @export
.as.SrcExprIntlString.SrcExpr <- function(x, ...) {
    args    <- .parseTranslateCallArgs(x$parsed)
    string  <- .sanitize(paste0(args$text, collapse = args$concat))
    intlStr <- c(x,
        string     = string,
        stringHash = .hashString(string),
        stringLang = args$srcLang)

    class(intlStr) <- c("SrcExprIntlString", class(x))
    return(intlStr)
}

.isTranslateCall <- function(x, ...) {
    UseMethod(".isTranslateCall")
}

#' @export
.isTranslateCall.SrcExpr <- function(x, ...) {
    return(is.call(x <- x$parsed) && .isTranslateCall.call(x))
}

#' @export
.isTranslateCall.call <- function(x, ...) {
    # Calling a function with backticks or quotes
    # is syntactically valid. transltr::translate
    # and transltr::"translate" are two distinct
    # calls because translate becomes an argument
    # of `::`() as a name or a string.
    return(
        identical(x[[1L]], quote(translate)) ||
        identical(x[[1L]], quote(transltr::translate)) ||
        identical(x[[1L]], quote(transltr::"translate")))
}

.parseTranslateCallArgs <- function(cl = call("translate")) {
    cl <- match.call(translate, cl, expand.dots = FALSE)

    return(
        list(
            text    = .evalArg(cl$text,    "text")    %??% "",
            concat  = .evalArg(cl$concat,  "concat")  %??% getDefaultConcat(),
            srcLang = .evalArg(cl$srcLang, "srcLang") %??% getDefaultSrcLang()))
}

.evalArg <- function(expr, name = character(1L)) {
    # FIXME: where to eval expr (arg)?
    return(
        tryCatch(eval(expr), error = \(err) {
            msg  <- err$message
            arg  <- deparse1(substitute(expr, parent.frame())[[3L]])
            expr <- .deparseAndTrim(expr)
            dot  <- if (endsWith(msg, ".")) "" else "."

            stop(
                sprintf(
                    "while evaluating argument '%s = %s': %s%s",
                    arg, expr, msg, dot),
                call. = FALSE)
        }))
}

.hashString <- function(string) {
    return(
        string |>
            charToRaw() |>
            sodium::sha256() |>
            sodium::bin2hex())
}

.sanitize <- function(x = character(1L)) {
    return(gsub("[\n\t\r ]+", " ", x))
}

.deparseAndTrim <- function(expr, maxLength = 50L) {
    if (nchar(string <- deparse1(expr)) > maxLength) {
        return(paste0(strtrim(string, maxLength), "..."))
    }

    return(string)
}
