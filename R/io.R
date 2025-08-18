#' A safe file lock, auto unlock
#' @param path file path
#' @param FUN function to be applied, first argument is the file path
#' @param ... other arguments passed to \code{FUN}
#' @param exclusive whether to lock exclusively, default is \code{TRUE}, passed to \code{\link[filelock]{lock}}
#' @param timeout lock timeout in milliseconds, default is \code{5000}, passed to \code{\link[filelock]{lock}}
#' @importFrom filelock lock unlock
#' @export
file_lock <- function(path, FUN, ..., exclusive = TRUE, timeout = 5000) {
  FUN <- match.fun(FUN)
  lock_file <- paste0(path, ".lock")
  lock <- lock(lock_file, exclusive = exclusive, timeout = timeout)
  unlock <- unlock
  if (is.null(lock)) {
    stop(paste0("The file lock cannot be obtained: ", lock_file))
  } else {
    res <- tryCatch(
      forceAndCall(1, FUN, path, ...),
      error = function(e) stop(e),
      finally = unlock(lock)
    )
  }
  invisible(res)
}
