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

    # Attach aliases, and small dev tools.
    # Names are as small as possible by design.
    attach(name = "tools:transltr:dev", what = local({
        # Exhaustive build checks.
        .ck  <- \() devtools::check(remote = TRUE, manual = TRUE)
        .man <- \() devtools::build_manual()

        # Shorter aliases.
        .mb <- microbenchmark::microbenchmark

        # Test a particular file.
        # Omit prefix `test-` and file extension.
        .tf <- \(file = "") {
            path <- file.path("tests", "testthat", sprintf("test-%s.R", file))
            return(testthat::test_file(path))
        }

        # Profile R code.
        # Call .pf() to start the profiler and .pf(NULL) to stop it.
        .pf <- \(file = ".Rprof.out") {
            return(
                Rprof(
                    filename         = file,
                    interval         = 0.01,
                    memory.profiling = TRUE,
                    line.profiling   = TRUE))
        }

        # View results stemming from R profiler.
        .sp <- \(file = ".Rprof.out") {
            out <- summaryRprof(
                filename  = file,
                chunksize = 5000,
                memory    = "both",
                lines     = "show")$by.line

            View(out, "R Profiler Results")
            return(invisible(out))
        }

        # Return this local environment
        # (to attach it to the search path).
        environment()
    }))
}
