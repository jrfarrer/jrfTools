#' Logger
#'
#' Creates a logger function
#'
#' @param msg message text
#' @param level classification of the message
#' @param file file to write to
#'
#' @return None
#'
#' @examples
#' fn_logger("Hello world")
#'
#' @export
fn_logger <- function(msg, level = "info", file = log_file) {
    cat(paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S.%OS"), "][", level, "] ", msg, "\n"), file = stdout())
}
