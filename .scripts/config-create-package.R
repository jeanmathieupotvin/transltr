#' Create a skeleton for the package
#'
#' Sections Setup, Creation, and Configuration are kept for reference purposes
#' and should be considered as read-only. They were ran once to create the
#' skeleton.
#'
#' Further sections may be re-ran and/or updated.
#'
#' @author Jean-Mathieu Potvin (<jm@@potvin.xyz>)
#'
#' @seealso [The usethis package](https://usethis.r-lib.org/)
NULL


# Setup ------------------------------------------------------------------------


install.packages("usethis")


# Creation ---------------------------------------------------------------------


usethis::create_package("transltr",
    open       = FALSE,
    rstudio    = FALSE,
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
                email   = "jm@potvin.xyz",
                role    = c("aut", "cre"),
                comment = c(ORCID = "0000-0002-8237-422X")),
            utils::person(
                given   = "Jérôme",
                family  = "Lavoué",
                email   = "jerome.lavoue@umontreal.ca",
                role    = "ctb",
                comment = c(ORCID = "0000-0003-4950-5475")))',
        Description = "
            A collection of translation mechanisms. Detect messages to be
            be translated in an application, organize their corresponding
            translated messages, and seamlessly use them."))


# Configuration ----------------------------------------------------------------


usethis::use_testthat()
usethis::use_coverage("codecov", "jeanmathieupotvin/transltr")
usethis::use_github_action("check-standard")
usethis::use_github_action("test-coverage")
usethis::use_github_actions_badge("check-standard", "jeanmathieupotvin/transltr")


# .Rprofile --------------------------------------------------------------------


usethis::use_devtools()
usethis::use_usethis()
usethis::use_partial_warnings()


# .gitignore -------------------------------------------------------------------


usethis::use_git_ignore(ignores = c(
    ".github/*.html",
    ".Rproj.user",
    ".Rhistory",
    ".RData",
    ".Renviron",
    "*.code-workspace"))


# .Rbuildignore ----------------------------------------------------------------


usethis::use_build_ignore(escape = FALSE, files = "[.]code-workspace$")
usethis::use_build_ignore(escape = TRUE,  files = c(
    ".git",
    ".github",
    ".scripts",
    ".gitignore",
    ".Rprofile",
    "codecov.yml"))


# LICENSE ----------------------------------------------------------------------


usethis::use_mit_license("Jean-Mathieu Potvin")


# NEWS -------------------------------------------------------------------------


usethis::use_news_md(open = FALSE)


# README -----------------------------------------------------------------------


usethis::use_readme_md(open = FALSE)
usethis::use_cran_badge()
usethis::use_lifecycle_badge("experimental")


# File R/transltr-package.R ----------------------------------------------------


usethis::use_package_doc(open = FALSE)
