#' Mock source headers
#'
#' These dummy source headers are used to test the behavior of
#' [from_tsf_header()] and related lower-level subfunctions.
#'
#' @usage
#' ## Expected usage in tests/testthat/test-src-header.R
#' source(get_mock_path("tsf-v1-headers"), environment())
#'
#' @note
#' The objects' names are purposely verbose to improve readability
#' of the unit tests that use them.
#'
NULL


# Mock headers for multiple functions ------------------------------------------


mock_tsf_header_v1_minimal <- c(
    "",
    "---",
    "template_version: 1",
    "generated_by: R package transltr 0.0.1",
    "generated_on: August 22, 2024 @ 08:00 (UTC)",
    "hash_algorithm: blake2b",
    "hash_length: 32",
    "language_keys:",
    "    en: English",
    "---",
    "")


# extract_tsf_header() ---------------------------------------------------------


mock_tsf_header_with_comments <- c(
    "# A comment to be ignored that says the header starts here",
    "---",
    "template_version: 1",
    "# A comment that may give further details on a field",
    "generated_by: R package transltr 0.0.1",
    "generated_on: August 22, 2024 @ 08:00 (UTC)",
    "hash_algorithm: blake2b",
    "hash_length: 32",
    "language_keys:",
    "    en: English",
    "    jp: 日本語",
    "---",
    "")

mock_tsf_header_no_sep_start <- c(
    "template_version: 1",
    "generated_by: R package transltr 0.0.1",
    "generated_on: August 22, 2024 @ 08:00 (UTC)",
    "hash_algorithm: blake2b",
    "hash_length: 32",
    "---")

mock_tsf_header_no_sep_end <- c(
    "---",
    "template_version: 1",
    "generated_by: R package transltr 0.0.1",
    "generated_on: August 22, 2024 @ 08:00 (UTC)",
    "hash_algorithm: blake2b",
    "hash_length: 32")

mock_tsf_header_no_header <- c("# no header", "")


# from_tsf_header() ------------------------------------------------------------


mock_tsf_header_duplicated_map_key <- c(
    "---",
    "generated_by: R package transltr 0.0.1",
    "generated_on: August 22, 2024 @ 08:00 (UTC)",
    "hash_algorithm: blake2b",
    "hash_length: 32",
    "language_keys:",
    "    en: English",
    "    en: French",
    "---")

mock_tsf_header_no_template_version <- c(
    "---",
    "generated_by: R package transltr 0.0.1",
    "generated_on: August 22, 2024 @ 08:00 (UTC)",
    "hash_algorithm: blake2b",
    "hash_length: 32",
    "language_keys:",
    "    en: English",
    "---")


# from_tsf_header_v1() ---------------------------------------------------------


mock_tsf_header_v1 <- c(
    "",
    "---",
    "template_version: 1",
    "generated_by: R package transltr 0.0.1",
    "generated_on: August 22, 2024 @ 08:00 (UTC)",
    "hash_algorithm: blake2b",
    "hash_length: 32",
    "language_keys:",
    "    en: English",
    "    fr: Français",
    "---",
    "")

mock_tsf_header_v1_with_further_fields <- c(
    "",
    "---",
    "project: transltr",
    "description: An example of a translation Markdown file (version 1)",
    "template_version: 1",
    "generated_by: R package transltr 0.0.1",
    "generated_on: August 22, 2024 @ 08:00 (UTC)",
    "hash_algorithm: blake2b",
    "hash_length: 32",
    "language_keys:",
    "    en: English",
    "    fr: Français",
    "---",
    "")
