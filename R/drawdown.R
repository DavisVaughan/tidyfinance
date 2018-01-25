#' Calculate the drawdown for a vector of returns
#'
#' `drawdown()` calculates a vector of drawdowns. `drawdown_max()` calculates
#' the single largest negative drawdown over the period of returns specified.
#'
#' @details
#'
#' Drawdown is a measure of how long it takes to recoup losses after a fall
#' from a previous high. The more negative the drawdown, the larger the loss.
#' The longer it takes to get back to 0 drawdown, the longer you are
#' "underwater."
#'
#' @examples
#'
#'
#'
#' @export
drawdown <- function(.r, type = "arithmetic") {
  wealth <- cumulative_return(.r, type) + 1
  max_wealth <- cummax(wealth)
  drawdown <- wealth / max_wealth - 1
  drawdown
}


#' @export
drawdown_max <- function(.r, type = "arithmetic") {
  min(drawdown(.r, type))
}



