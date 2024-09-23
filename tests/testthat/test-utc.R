# Decompose the output of utc() into its individual components.
date_time        <- utc()
date_time_tokens <- strsplit(date_time, " |,|:")[[1L]]

month    <- date_time_tokens[[1L]]
day      <- date_time_tokens[[2L]]
day_sep  <- date_time_tokens[[3L]]
year     <- date_time_tokens[[4L]]
time_sep <- date_time_tokens[[5L]]
hours    <- date_time_tokens[[6L]]
mins     <- date_time_tokens[[7L]]
secs     <- date_time_tokens[[8L]]

# Define expected values. We don't go as far as testing whether
# the pair of current month and day of month make sense. We only
# test for the actual returned format.
exp_months_en <- c(
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

exp_days <- c(
    "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
    "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
    "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
    "31")

# This covers the next 12 years. It should be enough.
exp_years <- c(
    "2024", "2025", "2026", "2027", "2028", "2029",
    "2030", "2031", "2032", "2033", "2034", "2035")

exp_hours <- c(
    "00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
    "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
    "20", "21", "22", "23")

exp_mins_secs <- c(
    "00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
    "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
    "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
    "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
    "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
    "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
    "60")

test_that("utc() returns a character string", {
    expect_type(date_time, "character")
    expect_length(date_time, 1L)
})

test_that("utc() returns the current month's full name", {
    skip_if(Sys.getlocale("LC_TIME") != "C.UTF-8")
    expect_in(month, exp_months_en)
})

test_that("utc() returns current date in the expected format", {
    expect_in(day,  exp_days)
    expect_in(year, exp_years)
})

test_that("utc() returns current time in the expected format", {
    expect_in(hours, exp_hours)
    expect_in(mins,  exp_mins_secs)
    expect_in(secs,  exp_mins_secs)
})

test_that("utc() uses expected separators", {
    # Since we declare "," as a separator, strsplit()
    # removes it and the actual separator for days
    # should match an empty string.
    expect_identical(day_sep,  "")
    expect_identical(time_sep, "@")
})
