#' format call chain
#' @param trace call chain, a list of call strings,created by \code{sys.calls()}
#' @return \code{character(1)} formatted string
#' @export
parse_call_chain <- function(trace = sys.calls()) {
  paste(
    sprintf("Call %d: %s", seq_along(trace), trace),
    collapse = "\n"
  )
}

#' assert a condition and throw error if it is not TRUE
#' @param con condition to be asserted
#' @param error error message to be thrown
#' @param trace call chain to be printed in the error message, if not specified,
#' the call chain will be parsed from \code{sys.calls()} inside this function.
#' @export
assert <- function(con, error, trace = NULL) {
  if (is.null(trace)) {
    trace <- sys.calls()
  }
  if (isFALSE(con)) {
    call <- parse_call_chain(trace)
    stop(paste(call, "\n", error), call. = FALSE, domain = NA)
  }
}

#' judge if an object is a vector, and not a list
#' @param obj object to be checked
#' @return \code{TRUE} if it is a vector, \code{FALSE} otherwise
#' @export
is_vector <- function(obj) {
  is.vector(obj) && !is.list(obj)
}

#' check if an object is a vector, and not a list, and throw error if not
#' @param obj object to be checked
#' @param error error message
#' @param trace call chain to be printed in the error message, if not specified,
#' the call chain will be parsed from \code{sys.calls()} inside this function.
#' @export
check_vector <- function(
    obj,
    error = "Object must be a vector!",
    trace = NULL) {
  if (is.null(trace)) {
    trace <- sys.calls()
  }
  assert(is_vector(obj), error, trace = trace)
}

#' create a directory for a file, if it does not exist
#' @param file a character string specifying the path to a file
#' @return the path to the directory
#' @export
create_file_dir <- function(file) {
  path <- dirname(file)
  creat_dir(path)
  invisible(path)
}

#' create a directory, if it does not exist, have no warning
#' @param path a character string specifying the path to a directory
#' @return the path to the directory
#' @export
creat_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  invisible(path)
}
