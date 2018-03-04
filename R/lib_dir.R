

LIB_DIR <- NULL


.onLoad <- function (lib, pkg) {
  tf <- tempfile(pattern = "apicheck", tmpdir = normalizePath(tempdir()))
  dir.create(tf)
  LIB_DIR <<- getOption('apicheck.lib_dir', tf)
  if (LIB_DIR == tf && ! dir.exists(tf)) {
    warning("Could not create temporary directory for package caching",
      "Package download won't work. To workaround, use `set_lib_dir()` manually.")
  }
}


#' Specify library location
#'
#' This specifies where libraries will be downloaded to,
#' and resets the cache of installed library locations.
#'
#' @param lib_dir Path to a directory, or \code{NULL} to unset.
#' @param create  Logical. Try to create the directory if it doesn't exist.
#'
#' @details
#' If \code{lib_dir} is set to \code{NULL}, a subdirectory of \code{tempdir()} will be used.
#' \code{lib_dir} will be normalized via \code{\link{normalizePath}}.
#'
#' @return \code{set_lib_dir} invisibly returns the old library location, or \code{NULL} if none was set in options.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_lib_dir("~/.apicheck")
#' }
set_lib_dir <- function (lib_dir, create = FALSE) {
  notthere <- ! is.null(lib_dir) && ! dir.exists(lib_dir)
  if (notthere && create) notthere <- ! dir.create(lib_dir, recursive = TRUE)
  if (notthere) stop("Directory '", lib_dir, "' does not exist", if (create) " and could not be created")

  if (! is.null(lib_dir)) lib_dir <- normalizePath(lib_dir)
  x <- options('apicheck.lib_dir' = lib_dir)

  return(invisible(x$apicheck.lib_dir))
}


#' @return \code{get_lib_dir} returns the actual library location, whether or not an option has been set.
#'
#' @rdname set_lib_dir
#'
#' @family utility functions
#'
#' @export
#'
#' @examples
#' get_lib_dir()
get_lib_dir <- function () {
  getOption('apicheck.lib_dir', LIB_DIR)
}


#' Delete all files from the package cache
#'
#' This attempts to delete \emph{everything} in the package cache.
#'
#' @details
#' The package cache is under the directory specified by \code{getOption("apicheck.lib_dir")},
#' or, if that is unset, in a per-session temporary directory. You should use \code{\link{set_lib_dir}}
#' to change this rather than setting the option directly. Within this directory, subdirectories
#' are named like \code{package-version}, e.g. \code{longurl-0.3.0}. Within these subdirectories are
#' the actual installed libraries.
#'
#' @return TRUE if all files and directories could be removed, FALSE otherwise.
#'
#' @family utility functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clear_package_cache()
#' }
clear_package_cache <- function() {
  lib_dir <- get_lib_dir()
  ok <- TRUE
  for (obj in list.files(lib_dir, full.names = TRUE)) {
    if (dir.exists(obj))  ok <- ok && identical(unlink(obj, recursive = TRUE), 0L)
    if (file.exists(obj)) ok <- ok && file.remove(obj)
  }

  return(ok)
}
