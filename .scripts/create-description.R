#' Create and/or update file DESCRIPTION
#'
#' Update and run this script to create a new top-level file DESCRIPTION.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @seealso [The usethis package](https://usethis.r-lib.org/)


usethis::use_description(
    roxygen    = TRUE,
    check_name = TRUE,
    fields     = list(
        Package     = "transltr",
        Title       = "A framework to support many languages in any R application",
        Version     = "0.0.1.9002",
        Language    = "en",
        Encoding    = "UTF-8",
        Roxygen     = "list(markdown = TRUE, r6 = TRUE)",
        URL         = "https://github.com/jeanmathieupotvin/transltr",
        BugReports  = "https://github.com/jeanmathieupotvin/transltr/issues/new",
        `Authors@R` = 'c(
            utils::person(
                given   = "Jean-Mathieu",
                family  = "Potvin",
                email   = "jeanmathieupotvin@ununoctium.dev",
                role    = c("aut", "cre")),
            utils::person(
                given   = "Jérôme",
                family  = "Lavoué",
                email   = "jerome.lavoue@umontreal.ca",
                role    = c("ctb", "fnd", "rev"),
                comment = c(ORCID = "0000-0003-4950-5475")))',
        Description = "
            An internationalization framework that enables full support of
            many languages in any R application. Detect text to translate
            within a project. Import and manage translations with a
            user-friendly interface. Easily export source text that requires
            translation to a plain text format that fosters collaboration with
            other non-technical and external collaborators."))

usethis::use_mit_license("Jean-Mathieu Potvin")

usethis::use_testthat(parallel = FALSE)
usethis::use_package("digest")
usethis::use_package("R6")
usethis::use_package("utils")
usethis::use_package("yaml")

usethis::use_package("covr",           "Suggests")
usethis::use_package("devtools",       "Suggests")
usethis::use_package("lifecycle",      "Suggests")
usethis::use_package("microbenchmark", "Suggests")
usethis::use_package("usethis",        "Suggests")
usethis::use_package("withr",          "Suggests")
