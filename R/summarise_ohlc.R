#' Calculate OHLC prices
#'
#' `summarise_ohlc()` calculates new OHLC price columns from specified price columns.
#' `aggregate_ohlc()` takes an existing set of OHLC columns and reaggregates
#' them to a coarser periodicity (daily to monthly, etc). The easiest way to
#' reaggregate is to collapse your index to a different period, group by it,
#' and call `aggregate_ohlc()`.
#'
#' @details
#'
#' Both `summarise_ohlc()` and `aggregate_ohlc()` are wrappers around a common
#' [dplyr::summarise()] style call that calculates the Open (first), High (max),
#' Low (min) and Close (last) values. Neither function knows
#' how to transform to a new `period` on its own, so the way to aggregate from,
#' say, daily to monthly involves collapsing a daily column of dates to
#' monthly using [tibbletime::collapse_index()], grouping on the collapsed date,
#' and then calling `aggregate_ohlc()`. The same workflow applies to
#' `summarise_ohlc()` when doing the similar calculation of  immediately creating
#'  monthly OHLC summaries from a daily price column.
#'
#' @param .tbl_time A tbl_time object.
#' @param ... Bare column names of the price columns to be converted to OHLC
#' format.
#' @param open,high,low,close Bare column names of existing OHLC columns to be
#' reaggregated.
#'
#' @examples
#'
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' library(tibbletime)
#' library(dplyr)
#'
#' data(FB)
#' FB_time <- FB %>%
#'   as_tbl_time(date)
#'
#' # ---------------------------------------------------------------------------
#' # Calculate monthly OHLC
#'
#' FB_monthly <- FB_time %>%
#'   collapse_by("monthly") %>%
#'   group_by(date) %>%
#'   summarise_ohlc(adjusted)
#'
#' FB_monthly
#'
#' # ---------------------------------------------------------------------------
#' # Reaggregate existing monthly OHLC up to yearly OHLC
#'
#' FB_monthly %>%
#'   collapse_by("yearly") %>%
#'   group_by(date) %>%
#'   aggregate_ohlc(adjusted_open, adjusted_high, adjusted_low, adjusted_close)
#'
#' @rdname summarise_ohlc
#' @export
#'
summarise_ohlc <- function(.tbl_time, ...) {
  UseMethod("summarise_ohlc")
}

#' @export
summarise_ohlc.tbl_time <- function(.tbl_time, ...) {

  .vars <- tidyselect::vars_select(names(.tbl_time), !!! rlang::quos(...))

  dplyr::summarise_at(
    .tbl  = .tbl_time,
    .vars = .vars,
    .funs = dplyr::funs(
      open  = dplyr::first(.),
      high  = max(.),
      low   = min(.),
      close = dplyr::last(.)
    )
  )
}

#' @rdname summarise_ohlc
#' @export
summarize_ohlc <- summarise_ohlc

#' @rdname summarise_ohlc
#' @export
aggregate_ohlc <- function(.tbl_time, open, high, low, close) {

  open  <- rlang::enquo(open)
  high  <- rlang::enquo(high)
  low   <- rlang::enquo(low)
  close <- rlang::enquo(close)

  qn <- rlang::quo_name

  dplyr::summarise(
    .data = .tbl_time,
    !!qn(open)  := dplyr::first(!!open),
    !!qn(high)  := max(!!high),
    !!qn(low)   := min(!!low),
    !!qn(close) := dplyr::last(!!close)
  )

}
