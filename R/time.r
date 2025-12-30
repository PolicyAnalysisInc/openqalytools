#' Get Days Per Year
#' 
#' Traverses up environments until it can find days
#' per year defined.
#' 
#' @return length one numeric containing days per year
#' 
#' @export
#'
#' @tests
#' # Default value when no parent frame context
#' expect_equal(
#'   get_dpy(),
#'   365
#' )
#'
#' # Test with parent frame containing cycle_length variables
#' expect_equal(
#'   local({
#'     cycle_length_days <- 365.25
#'     cycle_length_years <- 1
#'     get_dpy()
#'   }),
#'   365.25
#' )
#'
#' # Test with different days per year (e.g., 360-day year)
#' expect_equal(
#'   local({
#'     cycle_length_days <- 30
#'     cycle_length_years <- 1/12
#'     get_dpy()
#'   }),
#'   360
#' )
#'
#' # Test with vector inputs (uses first element)
#' expect_equal(
#'   local({
#'     cycle_length_days <- c(365, 730)
#'     cycle_length_years <- c(1, 2)
#'     get_dpy()
#'   }),
#'   365
#' )
#'
get_dpy <- function() {
  dpy <- 365
  for(i in 1:10) {
    try({
      pf <- parent.frame(i)
      cl_d <- pf$cycle_length_days
      cl_y <- pf$cycle_length_years
      if (!is.null(cl_d) && !is.null(cl_y)) {
        dpy <- (cl_d / cl_y)[1]
        break
      }
    })
  }
  return(dpy)
}

#' Caclulate Time in Days
#' 
#' Takes a string representing a unit of time and
#' calculates the number of days contained per unit.
#' 
#' @param x Either "days", "weeks", "months", or "years".
#' @param days_per_year the number of days in a year to be used in conversions
#' 
#' @return the numer of days in the unit of time
#' 
#' @export
#'
#' @tests
#' # Days
#' expect_equal(
#'   time_in_days("days", 365),
#'   1
#' )
#'
#' # Weeks
#' expect_equal(
#'   time_in_days("weeks", 365),
#'   7
#' )
#'
#' # Months (365-day year)
#' expect_equal(
#'   time_in_days("months", 365),
#'   365/12
#' )
#'
#' # Years
#' expect_equal(
#'   time_in_days("years", 365),
#'   365
#' )
#'
#' # Case insensitivity
#' expect_equal(
#'   time_in_days("DAYS", 365),
#'   1
#' )
#' expect_equal(
#'   time_in_days("Days", 365),
#'   1
#' )
#' expect_equal(
#'   time_in_days("WEEKS", 365),
#'   7
#' )
#' expect_equal(
#'   time_in_days("Months", 365),
#'   365/12
#' )
#' expect_equal(
#'   time_in_days("YEARS", 365),
#'   365
#' )
#'
#' # Different days_per_year values
#' expect_equal(
#'   time_in_days("months", 360),
#'   30
#' )
#' expect_equal(
#'   time_in_days("years", 365.25),
#'   365.25
#' )
#'
#' # Invalid unit returns NULL
#' expect_null(
#'   time_in_days("invalid", 365)
#' )
#'
time_in_days <- function(x, days_per_year) {
  switch(
    tolower(x),
    "days" = 1,
    "weeks" = 7,
    "months" = days_per_year / 12,
    "years" = days_per_year
  )
}