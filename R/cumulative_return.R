#' Cumulative returns
#'
#' Various functions for aggregating returns. `cumulative_return()` calculates
#' a vector of cumulative returns. `total_return()` returns the total return
#' over the period.
#'
#' @param .r A vector of returns.
#' @inheritParams calculate_return
#'
#' @section Implementation:
#'
#' Arithmetic returns are compounded as:
#'
#' \deqn{(1 + r_1)(1 + r_2) ... (1 + r_n) - 1}
#'
#' Log returns are compounded as:
#'
#' \deqn{r_1 + r_2 + ... + r_n}
#'
#' `total_return()` is the last value of the cumulative return.
#'
#' @export
#'
cumulative_return <- function(.r, type = "arithmetic") {

  validate_type(type)

  cum_fun = switch(type,

                   # Arithmetic returns compound as (1 + r1)(1 + r2) - 1
                   "arithmetic" = function(.r) cumprod(.r + 1) - 1,

                   # Log returns are additive r1 + r2
                   "log"        = function(.r) cumsum(.r)
  )

  cum_fun(.r)
}

#' @rdname cumulative_return
#' @export
#'
total_return <- function(.r, type = "arithmetic") {
  ret <- cumulative_return(.r, type)
  ret[length(ret)]
}
