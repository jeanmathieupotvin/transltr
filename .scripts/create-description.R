#' Create and/or update file DESCRIPTION
#'
#' Update and run this script to create a new top-level file DESCRIPTION.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @seealso
#' [The usethis package](https://usethis.r-lib.org/)


usethis::use_description(
    roxygen    = TRUE,
    check_name = TRUE,
    fields     = list(
        Package     = "transltr",
        Title       = "Support Many Languages in R",
        Version     = "0.1.0",
        Language    = "en",
        Encoding    = "UTF-8",
        Roxygen     = "list(markdown = TRUE, r6 = TRUE)",
        URL         = "https://github.com/jeanmathieupotvin/transltr",
        BugReports  = "https://github.com/jeanmathieupotvin/transltr/issues",
        `Authors@R` = 'c(
            utils::person(
                given   = "Jean-Mathieu",
                family  = "Potvin",
                email   = "jeanmathieupotvin@ununoctium.dev",
                role    = c("aut", "cre", "cph")),
            utils::person(
                given   = "Jérôme",
                family  = "Lavoué",
                email   = "jerome.lavoue@umontreal.ca",
                role    = c("ctb", "fnd", "rev"),
                comment = c(ORCID = "0000-0003-4950-5475")))',
        Description = "
            An object model for source text and translations. Find and extract
            translatable strings. Provide translations and seamlessly retrieve
            them at runtime."))

# R >= 4.3.0 is required for |>, _, and \(...).
# R >= 4.2.0 is required for native UTF-8 support on Windows.
use_package("R", type = "Depends", min_version = "4.3")

usethis::use_testthat(parallel = FALSE)
usethis::use_package("digest")
usethis::use_package("R6")
usethis::use_package("utils")
usethis::use_package("stringi")
usethis::use_package("yaml")
usethis::use_package("covr",           "Suggests")
usethis::use_package("devtools",       "Suggests")
usethis::use_package("lifecycle",      "Suggests")
usethis::use_package("microbenchmark", "Suggests")
usethis::use_package("usethis",        "Suggests")
usethis::use_package("withr",          "Suggests")
