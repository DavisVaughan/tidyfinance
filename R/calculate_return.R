#' Calculate returns at specified intervals
#'
#' Calculate arithmetic or log returns at specified periods.
#'
#' @inheritParams tibbletime::partition_index
#' @param .tbl_time A `tbl_time` object
#' @param ... The columns to calculate returns for.
#' One or more unquoted column names separated by commas.
#' @param type Either `"arithmetic"` or `"log"` returns.
#' @param suffix For each column specified in `...`, this is the suffix that
#' is appended onto the name of the new column that corresponds to the return.
#'
#' @details
#'
#' These functions make no attempt to ensure that you have a full period in
#' your return calculations. This means that if you calculate monthly returns
#' from daily returns but you do not have a complete
#' month of returns for your first month, you may get a value that does not
#' make much sense. It is up to the user to keep this in mind!
#'
#' @name calculate_return
#'
#' @export
calculate_return <- function(.tbl_time, ..., type = "arithmetic",
                             period = "daily", start_date = NULL, suffix = "return") {
  UseMethod("calculate_return")
}

#' @export
calculate_return.default <- function(.tbl_time, ..., type = "arithmetic",
                             period = "daily", start_date = NULL, suffix = "return") {
  glue_stop_not_tbl_time(.tbl_time)
}

#' @export
calculate_return.tbl_time <- function(.tbl_time, ..., type = "arithmetic",
                             period = "daily", start_date = NULL, suffix = "return") {

  .vars      <- tidyselect::vars_select(names(.tbl_time), !!! rlang::quos(...))
  index_quo  <- tibbletime::get_index_quo(.tbl_time)

  # Must select something
  assert_.vars_selected(.vars)

  # Change periods. Including endpoints results in something we can
  # calculate correct returns on
  .tbl_time_periodized <- tibbletime::as_period(
    .tbl_time         = .tbl_time,
    period            = period,
    start_date        = start_date,
    side              = "end",
    include_endpoints = TRUE
  )

  # Add on returns
  .tbl_time_returns <- dplyr::mutate_at(
    .tbl  = .tbl_time_periodized,
    .vars = .vars,
    .funs = dplyr::funs(
      !! suffix := calculate_return_vector(., type = type)
    )
  )

  # Remove first row with 0 return (slice would reorder)
  # row_number <- dplyr::row_number
  # dplyr::filter(.tbl_time_returns, row_number() != 1L)
  .tbl_time_returns
}

calculate_return_vector <- function(.p, type = "arithmetic") {

  validate_type(type)

  if (type == "arithmetic") {

    # P1 / P0 - 1
    ret <- .p / dplyr::lag(.p) - 1

  } else if (type == "log") {

    # log(P1 / P0)
    ret <- log(.p / dplyr::lag(.p))

  }

  # Ensure first value is 0, not NA. Eases calculation
  ret[1] <- 0

  ret
}

validate_type <- function(type) {
  if(type != "arithmetic" & type != "log") {
    stop("Unsupported return calculation type.")
  }
}

assert_.vars_selected <- function(.vars) {
  assertthat::assert_that(
    length(.vars) > 0,
    msg = "Must select at least 1 column to calculate return for"
  )
}

