#' Calculate constant growth rates
#'
#' `annualize_return()` computes the constant annual return of a return stream
#' that is typically longer than one year. This is often called the Compound
#' Annual Growth Rate (CAGR). `periodize_return()` takes that
#' concept and generalizes it to any valid period.
#'
#' @inheritParams cumulative_return
#' @param from_period The period the returns are currently in.
#' @param to_period The period that you want to calculate a constant growth
#' rate for.
#'
#' @section Implementation:
#'
#' The general equation for `periodize_return()` solves for `r_period` in
#' either:
#'
#' - Arithmetic return equation
#'
#' \deqn{(1 + r_period) ^ multiplier = (1 + r_1)(1 + r_2) ... (1 + r_n) }
#'
#' - Log return equation
#'
#' \deqn{ r_period * multiplier = r_1 + r_2 + ... + r_n }
#'
#' where `r_period` is the constant compound growth ratio for the specified
#' `to_period`. The `multiplier` is the same for the arithmetic and log formulas
#' and is calculated as:
#'
#' \deqn{ multiplier = length(.r) * annualization_multiplier(to_period) / annualization_multiplier(from_period) }
#'
#' @details
#'
#' `annualize_return()` is a common special case of `periodize_return()` in which
#' the `to_period` argument is set to `"yearly"`.
#'
#' @export
#'
#' @examples
#'
#' data(FANG)
#'
#' FANG_time <- FANG %>%
#'   as_tbl_time(date) %>%
#'   dplyr::group_by(symbol)
#'
#' # Compound Annual Growth Rate (CAGR)
#' FANG_time %>%
#'   calculate_return(adjusted) %>%
#'   dplyr::summarise(CAGR = annualize_return(adjusted_return))
#'
#' # Compound Monthly Growth Rate (CMGR)
#' FANG_time %>%
#'   calculate_return(adjusted, period = "daily") %>%
#'   dplyr::summarise(
#'     CMGR = periodize_return(
#'       adjusted_return,
#'       from_period = "daily",
#'       to_period = "monthly"
#'     )
#'   )
#'
#' # Equivalent 6 month growth rate
#' FANG_time %>%
#'   calculate_return(adjusted, period = "monthly") %>%
#'   dplyr::summarise(
#'     CMGR = periodize_return(
#'       adjusted_return,
#'       from_period = "monthly",
#'       to_period = "6 month"
#'     )
#'   )
#'
#' # One other useful feature is to take a daily return series and
#' # find the single daily return value that would give equivalent
#' # results if compounded over the same number of days
#' FANG_time %>%
#'   calculate_return(adjusted, period = "daily") %>%
#'   dplyr::summarise(
#'     equivalent_constant_daily_return = periodize_return(
#'       adjusted_return,
#'       from_period = "daily",
#'       to_period = "daily"
#'     )
#'   )
#'
periodize_return <- function(.r, from_period = "daily",
                             to_period = "yearly", type = "arithmetic") {

  # Create a correct multiplier by first coercing to annual, then to the
  # new period
  from_period_to_annual_multiplier     <- length(.r) / annualization_multiplier(from_period)
  from_annual_to_new_period_multiplier <- annualization_multiplier(to_period)

  # To get to the new period from annual, you multiply the factors together
  # ex) monthly_from_daily = (X days / 252 days_in_year) * (12 months_in_year)
  multiplier <- from_period_to_annual_multiplier * from_annual_to_new_period_multiplier

  .r_total <- total_return(.r, type)

  # Create the correct compound growth rate formula
  periodize_fun = switch(type,
    # Arithmetic returns
    "arithmetic" = function(x, multiplier) (x + 1) ^ (1 / multiplier) - 1,

    # Log returns
    "log"        = function(x, multiplier) x / multiplier
  )

  .r_periodized <- periodize_fun(.r_total, multiplier)

  .r_periodized
}

#' @rdname periodize_return
#' @export
#'
annualize_return <- function(.r, from_period = "daily", type = "arithmetic") {
  periodize_return(.r = .r, from_period = from_period,
                   to_period = "yearly", type = type)
}
