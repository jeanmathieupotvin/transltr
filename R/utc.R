utc <- function() {
    return(format(Sys.time(), tz = "UTC", format = "%B %d, %Y @ %T (%Z)"))
}
