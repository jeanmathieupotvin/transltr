#' Developer's entry point
#'
#' This script is automatically executed by R whenever a new session is started.
#'
#' @note
#' Since package testthat uses non-interactive sessions, using interactive()
#' below allows test runs to run in clean environments.
#'
#' @seealso [Startup process](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html)


# Global options ---------------------------------------------------------------


options(
    warnPartialMatchArgs   = TRUE,
    warnPartialMatchDollar = TRUE,
    warnPartialMatchAttr   = TRUE)


# Development tools and utility functions --------------------------------------


if (interactive()) {
    # Attach development packages.
    suppressMessages({
        require(covr)
        require(devtools)
        require(lifecycle)
        require(microbenchmark)
        require(testthat)
        require(usethis)
        require(withr)
    })

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
    .tf <- function(file = "") {
        path <- file.path("tests", "testthat", sprintf("test-%s.R", file))
        return(testthat::test_file(path))
    }

    # Profile R code.
    # Call .pf() to start the profiler and .pf(NULL) to stop it.
    .pf <- function(file = ".Rprof.out") {
        return(
            Rprof(
                filename         = file,
                interval         = 0.01,
                memory.profiling = TRUE,
                line.profiling   = TRUE))
    }

    # View results stemming from R profiler.
    .sp <- function(file = ".Rprof.out") {
        out <- summaryRprof(
            filename  = file,
            chunksize = 5000,
            memory    = "both",
            lines     = "show")$by.line

        View(out, "R Profiler Results")
        return(invisible(out))
    }

    # Clear global environment, but keep aliases.
    .rm <- function() {
        objs <- ls(".GlobalEnv", all.names = TRUE)
        keep <- c(".mb", ".pf", ".re", ".rm", ".sp", ".tf")
        objs <- objs[-match(keep, objs, 0L)]
        return(rm(list = objs, envir = globalenv()))
    }
}
