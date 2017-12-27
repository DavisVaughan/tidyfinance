#' Aggregate returns
#'
#' Various functions for aggregate returns. `total_return()` and `cumulative_return()`
#' aggregate a vector of returns with no regards for the period associated with
#' it. `aggregate_return()` aggregates returns in a `tbl_time` object with a
#' specified aggregation period (e.g., daily up to yearly return).
#'
#'
#' @rdname return_aggregation
#'
#' @inheritParams cumulative_return
#' @inheritParams calculate_return
#' @param .tbl_time A `tbl_time` object
#' @param ... The columns to aggregate returns for.
#' One or more unquoted column names separated by commas.
#' Positive values select variables; negative values drop variables.
#'
#' @export
#'
aggregate_return <- function(.tbl_time, ..., type = "arithmetic", period = "daily", start_date = NULL) {

  .vars      <- tidyselect::vars_select(names(.tbl_time), !!! rlang::quos(...))
  index_quo  <- tibbletime::get_index_quo(.tbl_time)
  index_char <- tibbletime::get_index_char(.tbl_time)

  .tbl_time %>%
    dplyr::mutate(rlang::UQ(index_char) := tibbletime::collapse_index(!!index_quo, period, start_date, where = "end")) %>%
    dplyr::group_by(!!index_quo, add = TRUE) %>%
    dplyr::summarise_at(.vars = .vars,
                        .funs = dplyr::funs(total_return(., type = type)) )
}

#' @rdname return_aggregation
#' @export
#'
total_return <- function(.r, type = "arithmetic") {
  ret <- cumulative_return(.r, type)
  dplyr::last(ret)
}

#' @param .r A vector of returns.
#'
#' @rdname return_aggregation
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
