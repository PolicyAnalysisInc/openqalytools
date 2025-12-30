#' Convert Probability to Odds
#'
#' Converts a probability value to odds.
#'
#' @param p a probability to convert to odds
#'
#' @return numeric odds value
#'
#' @export
#'
#' @tests
#' # Basic conversions
#' expect_equal(
#'   prob_to_odds(0.25),
#'   1/3
#' )
#' expect_equal(
#'   prob_to_odds(0.5),
#'   1
#' )
#' expect_equal(
#'   prob_to_odds(0.75),
#'   3
#' )
#'
#' # Edge cases: boundaries
#' expect_equal(
#'   prob_to_odds(0),
#'   0
#' )
#' expect_equal(
#'   prob_to_odds(1),
#'   Inf
#' )
#'
#' # Vector input
#' expect_equal(
#'   prob_to_odds(c(0, 0.25, 0.5, 0.75, 1)),
#'   c(0, 1/3, 1, 3, Inf)
#' )
#'
#' # Numeric precision
#' expect_equal(
#'   prob_to_odds(0.1),
#'   0.1 / 0.9
#' )
#'
#' # Round-trip property: prob -> odds -> prob
#' expect_equal(
#'   odds_to_prob(prob_to_odds(0.33)),
#'   0.33
#' )
#'
prob_to_odds <- function(p) {
  ret <- vector(mode = 'numeric', length = length(p))
  one_index <- p == 1
  ret[one_index] <- Inf
  ret[!one_index] <- p[!one_index] / (1 - p[!one_index])
  ret
}

#' Convert Odds to Probability
#'
#' Converts odds to a probability value.
#'
#' @param odds odds to convert to probability
#'
#' @return numeric probability value between 0 and 1
#'
#' @export
#'
#' @tests
#' # Basic conversions
#' expect_equal(
#'   odds_to_prob(1/3),
#'   0.25
#' )
#' expect_equal(
#'   odds_to_prob(1),
#'   0.5
#' )
#' expect_equal(
#'   odds_to_prob(3),
#'   0.75
#' )
#'
#' # Edge cases: boundaries
#' expect_equal(
#'   odds_to_prob(0),
#'   0
#' )
#' expect_equal(
#'   odds_to_prob(Inf),
#'   1
#' )
#'
#' # Vector input
#' expect_equal(
#'   odds_to_prob(c(0, 1/3, 1, 3, Inf)),
#'   c(0, 0.25, 0.5, 0.75, 1)
#' )
#'
#' # Numeric precision
#' expect_equal(
#'   odds_to_prob(9),
#'   0.9
#' )
#'
#' # Round-trip property: odds -> prob -> odds
#' expect_equal(
#'   prob_to_odds(odds_to_prob(2.5)),
#'   2.5
#' )
#'
odds_to_prob <- function(odds) {
  ret <- vector(mode = 'numeric', length = length(odds))
  inf_index <- odds == Inf
  ret[inf_index] <- 1
  ret[!inf_index] <- odds[!inf_index] / (odds[!inf_index] + 1)
  ret
}