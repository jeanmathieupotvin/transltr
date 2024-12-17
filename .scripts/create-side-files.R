#' Create and/or update further side-files
#'
#' Update and run this script to create new versions of these files:
#'   * .codecov.yml,
#'   * .github/*,
#'   * LICENSE,
#'   * LICENSE.md,
#'   * README.md, and
#'   * R/transltr-package.R.
#'
#' @details
#' This script is kept mostly for audit purposes. It was previously executed,
#' and all the files it creates already exist. In normal circumstances, they
#' can be left as is.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @seealso
#' [The usethis package](https://usethis.r-lib.org/)


# Activate Codecov and create .codecov.yml.
usethis::use_coverage("codecov", "jeanmathieupotvin/transltr")

# Activate GitHub Actions and create .github/.
usethis::use_github_action("check-standard")
usethis::use_github_action("test-coverage")
usethis::use_github_actions_badge("check-standard", "jeanmathieupotvin/transltr")

# Create file README.md.
usethis::use_readme_md(open = FALSE)
usethis::use_cran_badge()
usethis::use_lifecycle_badge("experimental")

# Create cran-comments.md.
usethis::use_cran_comments()

# Create file R/transltr-package.R.
usethis::use_package_doc(open = FALSE)
