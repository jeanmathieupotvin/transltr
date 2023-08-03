# Setup ------------------------------------------------------------------------


translatableString <- TranslatableString$new("token1", "token2")


# Public methods ---------------------------------------------------------------


test_that("$initialize() returns an object of class TranslatableString", {
    expect_s3_class(TranslatableString$new(), c("TranslatableString", "R6"))
})

test_that("$initialize() throws an error if concat is invalid", {
    expect_error(TranslatableString$new(concat = 1L))
    expect_error(TranslatableString$new(concat = c(" ", " ")))
    expect_error(TranslatableString$new(concat = NA_character_))
    expect_snapshot_error(TranslatableString$new(concat = 1L))
})

test_that("$initialize() sets $tokens", {
    expect_identical(translatableString$tokens, c("token1", "token2"))
})

test_that("$initialize() sets $string", {
    expect_identical(translatableString$string, "token1 token2")
})

test_that("$initialize() assembles tokens into a character string", {
    # These expectations test various unusual structure of
    # ... to ensure $initialize() handles them appropriately.
    translatableString1 <- TranslatableString$new(
        list("token1", list("token2")), c("token3", "token4"))
    translatableString2 <- TranslatableString$new(
        c("token1", "token2", "token3", "token4"))
    translatableString3 <- TranslatableString$new(
        c("token1", "token2"), c("token3", "token4"))

    expect_identical(translatableString1$string, "token1 token2 token3 token4")
    expect_identical(translatableString2$string, "token1 token2 token3 token4")
    expect_identical(translatableString3$string, "token1 token2 token3 token4")
})

test_that("$initialize() throws an error if it cannot convert values", {
    expect_error(TranslatableString$new(\() { }))
    expect_snapshot_error(TranslatableString$new(\() { }))
})

test_that("$format() returns a named character of length 2", {
    translatableStringFormatted <- translatableString$format()

    expect_type(translatableStringFormatted, "character")
    expect_length(translatableStringFormatted, 2L)
    expect_named(translatableStringFormatted, c("header", "string"))
})

test_that("$format() throws an error if signatureLength is invalid", {
    expect_error(translatableString$format(-1L))
    expect_error(translatableString$format(65L))
    expect_snapshot_error(translatableString$format(-1L))
})

test_that("$format() formats header appropriately with signature", {
    # Signature is a predictable SHA-256 hash which
    # allows us to use a simple expect_identical().
    header0  <- translatableString$format( 0L)[["header"]]
    header16 <- translatableString$format(16L)[["header"]]
    header32 <- translatableString$format(32L)[["header"]]
    header64 <- translatableString$format(64L)[["header"]]

    expect_identical(header0,  "<TranslatableString>")
    expect_identical(header16, "<TranslatableString: c831e66d85ddb217>")
    expect_identical(header32, "<TranslatableString: c831e66d85ddb21722130ee9b02aedd6>")
    expect_identical(header64, "<TranslatableString: c831e66d85ddb21722130ee9b02aedd62ecca0eba2661acb9320057aa4aa568f>")
})

test_that("$format() uses full signature by default", {
    expect_identical(
        translatableString$format()[["header"]],
        "<TranslatableString: c831e66d85ddb21722130ee9b02aedd62ecca0eba2661acb9320057aa4aa568f>")
})

test_that("$print() returns object invisibly", {
    withr::with_output_sink(tempfile(), {
        expect_invisible(translatableString$print())
        expect_identical(translatableString$print(), translatableString)
    })
})

test_that("$print() prints signature and string as intended", {
    expect_snapshot_output({
        addNote("Using full $signature (by default):\n")
        translatableString$print()
    })
})

test_that("$print() applies signatureLength as intended", {
    expect_snapshot_output({
        addNote("Using 16 first characters of $signature:\n")
        translatableString$print(16L)
    })
    expect_snapshot_output({
        addNote("Not using $signature:\n")
        translatableString$print(0L)
    })
})

test_that("$cat() prints nothing if $string is empty", {
    expect_output(TranslatableString$new()$cat(), NA)
})

test_that("$cat() wraps $string if it is longer 60 characters", {
    translatableStringLong <- TranslatableString$new(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
        "Nunc ac urna eros.")

    translatableStringVeryLong <- TranslatableString$new(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
        "Nunc ac urna eros.",
        "Sed placerat risus et mauris semper, vel pulvinar lectus laoreet.",
        "Nullam interdum bibendum nunc, at iaculis nisi aliquam vel.",
        "Suspendisse potenti.",
        "Donec orci arcu, aliquam non sapien eget, bibendum malesuada arcu.",
        "Sed ut fermentum mi. Orci varius natoque penatibus et magnis dis",
        "parturient montes, nascetur ridiculus mus.",
        "Donec nulla justo, consectetur sed quam ac, lacinia commodo odio.")

    expect_snapshot_output({
        addNote("String below has 75 characters.")
        addNote("Each line should be indented by 1 space.\n")
        translatableStringLong$cat()
    })
    expect_snapshot_output({
        addNote("String below has 463 characters.")
        addNote("Each line should be indented by 1 space.\n")
        translatableStringVeryLong$cat()
    })
})

test_that("$asCharacter() returns $string", {
    expect_identical(
        translatableString$asCharacter()[[1L]],  # [[ drops attributes.
        translatableString$string)
})

test_that("$asCharacter() attaches $signature as an attribute to the output", {
    string <- translatableString$asCharacter()

    expect_length(attributes(string), 1L)
    expect_identical(attr(string, "signature"), translatableString$signature)
})

test_that("$asList() returns a list with all fields", {
    translatableStringList <- translatableString$asList()

    expect_type(translatableStringList, "list")
    expect_length(translatableStringList, 3L)
    expect_named(translatableStringList, c("signature", "string", "tokens"))

    expect_identical(translatableStringList$signature, translatableString$signature)
    expect_identical(translatableStringList$string,    translatableString$string)
    expect_identical(translatableStringList$tokens,    translatableString$tokens)
})


# Private methods --------------------------------------------------------------


test_that("$.sanitize() scrubs repeated blank characters", {
    # $.sanitize() is implicitly/partially
    # covered by tests for $initialize().
    expect_identical(
        TranslatableString$new("Repeated \n\n new lines.")$string,
        "Repeated new lines.")
    expect_identical(
        TranslatableString$new("Repeated \t\t tabs.")$string,
        "Repeated tabs.")
    expect_identical(
        TranslatableString$new("Repeated \r\r carriage returns.")$string,
        "Repeated carriage returns.")
    expect_identical(
        TranslatableString$new("Repeated    regular spaces.")$string,
        "Repeated regular spaces.")
})


# S3 ---------------------------------------------------------------------------


test_that("as.TranslatableString() works", {
    skip("s3 generic covered by test cases of its methods")
})

test_that("as.character.TranslatableString() calls $asCharacter()", {
    expect_identical(
        as.character(translatableString),
        translatableString$asCharacter())
})

test_that("as.list.TranslatableString() calls $asList()", {
    expect_identical(
        as.list(translatableString),
        translatableString$asList())
})
