#' Mock source headers
#'
#' These dummy sources headers are used to test the behavior of
#' [extract_src_header()] and [from_src_header()] regardless of
#' the underlying template's version.
#'
#' @usage
#' ## Within concerned test scripts
#' source(file.path("_mocks", "src-header.R"))
#'
#' @note
#' The objects' names are purposely verbose to improve readability
#' of the unit tests that use them.
#'
NULL

mock_src_header <- c(
    "",
    "---",
    "project: transltr",
    "description: An example of a translation Markdown file (version 1)",
    "template_version: 1",
    "generated_by: R package transltr 0.0.1",
    "generated_on: August 22, 2024 @ 08:00 UTC",
    "hash_algorithm: blake2b",
    "hash_length: 32",
    "hashes:",
    "    - \"60ed1cd2b78a3448e7fab38d5830e249\"",
    "    - \"9bbbb7410fa6464a1a6a216919179455\"",
    "language_keys:",
    "    en: English",
    "    fr: Français",
    "    es: Español",
    "    jp: 日本語",
    "---",
    ""
)

mock_src_header_no_sep_end <- c(
    "---",
    "template_version: 1",
    "generated_by: R package transltr 0.0.1",
    "generated_on: August 22, 2024 @ 08:00 UTC",
    "hash_algorithm: blake2b",
    "hash_length: 32",
    "hashes:",
    "    - \"60ed1cd2b78a3448e7fab38d5830e249\"",
    "language_keys:",
    "    en: English"
)

mock_src_header_no_sep_start <- c(
    "template_version: 1",
    "generated_by: R package transltr 0.0.1",
    "generated_on: August 22, 2024 @ 08:00 UTC",
    "hash_algorithm: blake2b",
    "hash_length: 32",
    "hashes:",
    "    - \"60ed1cd2b78a3448e7fab38d5830e249\"",
    "language_keys:",
    "    en: English",
    "---"
)

mock_src_header_no_header <- c("", "")

mock_src_header_duplicated_map_key <- c(
    "---",
    "generated_by: R package transltr 0.0.1",
    "generated_on: August 22, 2024 @ 08:00 UTC",
    "hash_algorithm: blake2b",
    "hash_length: 32",
    "hashes:",
    "    - \"60ed1cd2b78a3448e7fab38d5830e249\"",
    "language_keys:",
    "    en: English",
    "    en: French",
    "---"
)

mock_src_header_v1_no_template_version <- c(
    "---",
    "generated_by: R package transltr 0.0.1",
    "generated_on: August 22, 2024 @ 08:00 UTC",
    "hash_algorithm: blake2b",
    "hash_length: 32",
    "hashes:",
    "    - \"60ed1cd2b78a3448e7fab38d5830e249\"",
    "language_keys:",
    "    en: English",
    "---"
)
