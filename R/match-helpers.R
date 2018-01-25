#' Helpers for matching common financial columns
#'
#' Extensions of the family of "select helpers" from `tidyselect` like
#' [tidyselect::contains()] and [tidyselect::matches()]. `matches_ohlca()` selects the
#' columns `open`, `high`, `low`, `close`, and `adjusted`. `matches_ohlcav()`
#' additionally matches `volume`.
#'
#' @details
#'
#' As a user, you should rarely, if ever, need to specify `vars`.
#'
#' @examples
#'
#' data(FANG)
#'
#' dplyr::select(FANG, matches_ohlca())
#'
#' FANG %>%
#'   as_tbl_time(date) %>%
#'   dplyr::group_by(symbol) %>%
#'   calculate_return(matches_ohlca())
#'
#' @rdname match_helpers
#' @export
#'
matches_ohlca <- function(vars = NULL) {
  if(is.null(vars)) vars <- tidyselect::peek_vars()
  tidyselect::matches("open|high|low|close|adjusted", ignore.case = TRUE, vars = vars)
}

#' @rdname match_helpers
#' @export
matches_ohlcav <- function(vars = NULL) {
  if(is.null(vars)) vars <- tidyselect::peek_vars()
  tidyselect::matches("open|high|low|close|adjusted|volume", ignore.case = TRUE, vars = vars)
}
