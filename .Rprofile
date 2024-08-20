#' Developer's entry point
#'
#' This script is automatically executed by R whenever a new session is started.
#'
#' @seealso [Startup process](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html)


# Global options ---------------------------------------------------------------


options(
    warnPartialMatchArgs   = TRUE,
    warnPartialMatchDollar = TRUE,
    warnPartialMatchAttr   = TRUE)


# Development tools and utility functions --------------------------------------


# Since testthat uses non-interactive sessions, using interactive()
# below allows test runs to run in clean environments.
if (interactive()) {
    # Attach development packages.
    suppressMessages(require(covr))
    suppressMessages(require(devtools))
    suppressMessages(require(microbenchmark))
    suppressMessages(require(usethis))
    suppressMessages(require(withr))

    # Create a shorter alias for microbenchmark::microbenchmark().
    .mb <- microbenchmark::microbenchmark

    # Generate a plain text coverage report (file COVERAGE) with covr.
    .re <- function() {
        on.exit(close(con))
        con <- file("COVERAGE", "wt", FALSE, "UTF-8")
        cov <- covr::package_coverage()

        sink(con, type = "message")
        print(cov)
        sink(NULL, type = "message")
        return(invisible())
    }

    # Test a particular file. Omit prefix `test-` and file extension.
    .tf <- function(file = character(1L)) {
        path <- file.path("tests", "testthat", sprintf("test-%s.R", file))
        return(testthat::test_file(path))
    }

    # Clear global environment, but keep aliases.
    .rm <- function() {
        objs <- ls(".GlobalEnv", all.names = TRUE)
        keep    <- c(".mb", ".rm", ".tf")
        objs <- objs[-match(keep, objs, 0L)]

        return(rm(list = objs, envir = globalenv()))
    }
}
