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
        Title       = "A general purpose translator for R",
        Version     = "0.0.1",
        License     = "MIT + LICENSE",
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
            A collection of translation mechanisms. Detect messages to be
            be translated in an application, organize their corresponding
            translated messages, and seamlessly use them."))

usethis::use_testthat()
usethis::use_package("R6")
