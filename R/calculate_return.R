#' Calculate returns at specified intervals
#'
#' Calculate arithmetic or log returns at specified periods.
#'
#' @inheritParams tibbletime::partition_index
#' @param .tbl_time A `tbl_time` object
#' @param ... The columns to calculate returns for.
#' One or more unquoted column names separated by commas.
#' Positive values select variables; negative values drop variables.
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
                             period = "daily", start_date = NULL,
                             suffix = "return") {

  .vars      <- tidyselect::vars_select(names(.tbl_time), !!! rlang::quos(...))
  row_number <- dplyr::row_number # some problem with using this in the filter
  index_quo  <- tibbletime::get_index_quo(.tbl_time)

  # Must select something
  if(length(.vars) == 0) {
    glue_stop("Must select at least 1 column to calculate return for.")
  }

  .tbl_time %>%
    tibbletime::as_period(period = period, side = "end",
                          start_date = start_date, include_endpoints = TRUE) %>%
    dplyr::mutate_at(.vars = .vars,
                     .funs = dplyr::funs(rlang::UQ(suffix) := calculate_return_vector(., type = type))) %>%
    dplyr::filter(row_number() != 1L) %>%
    dplyr::select(!!!dplyr::groups(.tbl_time), !!index_quo, tidyselect::one_of(paste0(.vars, "_", suffix)))
}

#' @param .p A vector of prices.
#'
#' @rdname calculate_return
#' @export
calculate_return_vector <- function(.p, type = "arithmetic") {

  validate_type(type)

  if (type == "arithmetic") {

    # P1 / P0 - 1
    ret <- .p / dplyr::lag(.p) - 1

  } else if (type == "log") {

    # log(P1 / P0)
    ret <- log(.p / dplyr::lag(.p))

  }

  ret
}

validate_type <- function(type) {
  if(type != "arithmetic" & type != "log") {
    stop("Unsupported return calculation type.")
  }
}
