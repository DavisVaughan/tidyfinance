# Reexports --------------------------------------------------------------------

#' @importFrom tibbletime as_tbl_time
#' @export
#'
tibbletime::as_tbl_time

#' @importFrom dplyr %>%
#' @export
#'
dplyr::`%>%`

#' @importFrom rlang :=
#'
rlang::`:=`

# Globals ----------------------------------------------------------------------

globalVariables(".")

# Utils ------------------------------------------------------------------------


glue_stop <- function(..., .sep = "") {
  stop(glue::glue(..., .sep, .envir = parent.frame()), call. = FALSE)
}

glue_stop_not_tbl_time <- function(x) {
  classes <- paste(class(x), collapse = ", ")

  stop(glue::glue("Object is of class '{classes}',",
                  " but must be of class 'tbl_time'"),
       call. = FALSE)
}
