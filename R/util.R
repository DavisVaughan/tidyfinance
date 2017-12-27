# Reexports --------------------------------------------------------------------

#' @importFrom tibbletime as_tbl_time
#' @export
#'
tibbletime::as_tbl_time

#' @importFrom dplyr %>%
#' @export
#'
dplyr::`%>%`

glue_stop <- function(..., .sep = "") {
  stop(glue::glue(..., .sep, .envir = parent.frame()), call. = FALSE)
}
