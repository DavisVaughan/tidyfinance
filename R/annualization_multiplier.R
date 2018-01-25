#' Retrieve the annualization multiplier
#'
#' Specify either a character annualization period or a numeric annualization multiplier of
#' your own.
#'
#' @param period The period at which to calculate an annualization factor for.
#' Either a supported character ("day", "week", "month", "quarter", "year") or a numeric.
#'
#' @details
#'
#' The following character annualization specifications are supported.
#'
#' - "day"     = 252
#' - "week"    = 52
#' - "month"   = 12
#' - "quarter" = 4
#' - "year"    = 1
#'
#' @examples
#'
#' annualization_multiplier("day")
#' annualization_multiplier("2 day")
#' annualization_multiplier(250)
#'
#' @export
#'
annualization_multiplier <- function(period) {

  # If the user specified their own numeric annualization period, return it
  if(is.numeric(period)) {
    return(period)
  }

  period_list <- tibbletime::parse_period(period)

  multiplier <- switch(period_list[["period"]],
    "day"     = 252,
    "week"    = 52,
    "month"   = 12,
    "quarter" = 4,
    "year"    = 1,
    glue_stop("{period} is not a supported annualization specification.")
  )

  multiplier <- multiplier / period_list[["freq"]]

  multiplier
}
