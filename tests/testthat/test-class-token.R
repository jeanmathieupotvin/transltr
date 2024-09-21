# token() ----------------------------------------------------------------------


test_that("token() returns a S3 object of class Token", {
    token <- token("type", "value")

    expect_type(token, "list")
    expect_length(token, 2L)
    expect_s3_class(token, "Token")
    expect_identical(token$type, "type")
    expect_identical(token$value, "value")
})

test_that("token() includes ... arguments in objects", {
    expect_identical(token(test_field = "test")$test_field, "test")
})

test_that("token() passes argument .super to class vector", {
    super_token <- token(.super = c("SuperSuperToken", "SuperToken"))
    expect_s3_class(super_token, c("SuperSuperToken", "SuperToken", "Token"))
})


# tsf_block_line_token() -------------------------------------------------------


test_that("tsf_block_line_token() returns a S3 object of class SrcBlockLineToken", {
    token <- tsf_block_line_token("NULL", "value", "subtype")

    expect_type(token, "list")
    expect_length(token, 3L)
    expect_s3_class(token, c("SrcBlockLineToken", "Token"))
    expect_identical(token$type, "NULL")
    expect_identical(token$subtype, "subtype")
    expect_identical(token$value, "value")
})

test_that("tsf_block_line_token() sets subtype equal to type if null", {
    expect_identical(tsf_block_line_token("TXT")$subtype, "TXT")
})

test_that("tsf_block_line_token() validates argument type", {
    expect_error(tsf_block_line_token("BAD_TYPE"))
    expect_snapshot(tsf_block_line_token("BAD_TYPE"), error = TRUE)
})

test_that("tsf_block_line_token() validates argument value", {
    expect_error(tsf_block_line_token(value = 1L))
    expect_snapshot(tsf_block_line_token(value = 1L), error = TRUE)
})

test_that("tsf_block_line_token() validates argument subtype", {
    expect_error(tsf_block_line_token(subtype = 1L))
    expect_snapshot(tsf_block_line_token(subtype = 1L), error = TRUE)
})
