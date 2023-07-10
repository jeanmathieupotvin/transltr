#' Developer's entry point
#'
#' This script is automatically executed by R whenever a new session is started.
#'
#' @seealso [Startup](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html)


# Attach development packages --------------------------------------------------


if (interactive()) {
    suppressMessages(require(devtools))
    suppressMessages(require(microbenchmark))
    suppressMessages(require(usethis))
}


# Global options ---------------------------------------------------------------


options(
    warnPartialMatchArgs   = TRUE,
    warnPartialMatchDollar = TRUE,
    warnPartialMatchAttr   = TRUE)


# Miscellaneous development utilities ------------------------------------------


# Create useful aliases and very small helper functions.
# .mb() : alias for microbenchmark::microbenchmark().
# .rm() : clear global environment, but keep aliases.
# .tf() : test a particular file. Omit prefix `test-` and file extension.

if (interactive()) {
    .mb <- microbenchmark::microbenchmark

    .rm <- function() {
        objs <- ls(".GlobalEnv", all.names = TRUE)
        keep    <- c(".mb", ".rm", ".tf")
        objs <- objs[-match(keep, objs, 0L)]

        return(rm(list = objs, envir = globalenv()))
    }

    .tf <- function(file = character(1L)) {
        path <- file.path("tests", "testthat", sprintf("test-%s.R", file))
        return(testthat::test_file(path))
    }
}
