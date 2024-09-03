# Decompose the output of utc() into its individual components.
test_date_time        <- utc()
test_date_time_tokens <- strsplit(test_date_time, " |,|:")[[1L]]

test_month    <- test_date_time_tokens[[1L]]
test_day      <- test_date_time_tokens[[2L]]
test_day_sep  <- test_date_time_tokens[[3L]]
test_year     <- test_date_time_tokens[[4L]]
test_time_sep <- test_date_time_tokens[[5L]]
test_hours    <- test_date_time_tokens[[6L]]
test_mins     <- test_date_time_tokens[[7L]]
test_secs     <- test_date_time_tokens[[8L]]

# Define expected values. We don't go as far as testing whether
# the pair of current month and day of month make sense. We only
# test for the actual returned format.
expected_months_en <- c(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December")

expected_days <- c(
    "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
    "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
    "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
    "31")

# This covers the next 12 years. It should be enough.
expected_years <- c(
    "2024", "2025", "2026", "2027", "2028", "2029",
    "2030", "2031", "2032", "2033", "2034", "2035")

expected_hours <- c(
    "00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
    "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
    "20", "21", "22", "23")

expected_mins_secs <- c(
    "00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
    "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
    "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
    "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
    "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
    "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
    "60")

test_that("utc() returns a character string", {
    expect_type(test_date_time, "character")
    expect_length(test_date_time, 1L)
})

test_that("utc() returns the current month's full name", {
    skip_if(Sys.getlocale("LC_TIME") != "C.UTF-8")
    expect_in(test_month, expected_months_en)
})

test_that("utc() returns current date in the expected format", {
    expect_in(test_day,   expected_days)
    expect_in(test_year,  expected_years)
})

test_that("utc() returns current time in the expected format", {
    expect_in(test_hours, expected_hours)
    expect_in(test_mins,  expected_mins_secs)
    expect_in(test_secs,  expected_mins_secs)
})

test_that("utc() uses expected separators", {
    # Since we declare "," as a separator, strsplit()
    # removes it and the actual separator for days
    # should match an empty string.
    expect_identical(test_day_sep,  "")
    expect_identical(test_time_sep, "@")
})
