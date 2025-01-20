test_that("it returns a character string", {
    out_sha1 <- hash("en", "Hello, world!", "sha1")
    out_utf8 <- hash("en", "Hello, world!", "utf8")
    expect_type(out_sha1, "character")
    expect_type(out_utf8, "character")
    expect_length(out_sha1, 1L)
    expect_length(out_utf8, 1L)
})

test_that("it returns null for unknown hashing algorithms", {
    expect_null(hash("en", "Hello, world!", "error"))
})

test_that("it returns a sha-1 hash wheen algorithm is sha1", {
    # Expected SHA-1 hashes were generated externally
    # by https://codebeautify.org/sha1-hash-generator and
    # double-checked by https://10015.io/tools/sha1-encrypt-decrypt.

    long_string <- normalize("
        Lorem Ipsum is simply dummy text of the printing and typesetting industry.
        Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,
        when an unknown printer took a galley of type and scrambled it to make a type
        specimen book. It has survived not only five centuries, but also the leap into
        electronic typesetting, remaining essentially unchanged. It was popularised in
        the 1960s with the release of Letraset sheets containing Lorem Ipsum passages,
        and more recently with desktop publishing software like Aldus PageMaker
        including versions of Lorem Ipsum.")

    expect_identical(hash("en", long_string,       "sha1"), "ce06fb763aad57fd2b265a18a375d7daae86af7f")
    expect_identical(hash("en", "Hello, world!",   "sha1"), "256e0d707386d0fcd9abf10ad994000bdaa25812")
    expect_identical(hash("fr", "Bonjour, monde!", "sha1"), "f3c8754329c1b152887d35f00119fca783243d27")
    expect_identical(hash("es", "¡Hola Mundo!",    "sha1"), "faf516ddb9969506f4a8771d528efb029db50698")
    expect_identical(hash("ja", "こんにちは世界！", "sha1"), "83daa9cda6da5189dc5c81c78323361fab6b652b")
})

test_that("it returns an integer hash wheen algorithm is utf8", {
    expect_identical(hash("en", "Hello, world!", "utf8"), "12351")
})
