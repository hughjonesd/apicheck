

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
#' `set_lib_dir()` specifies where packages will be downloaded to. `get_lib_dir()` returns this
#' directory. `clear_lib_dir()` deletes all downloaded packages.
#'
#' @param lib_dir Path to a directory, or `NULL` to unset.
#' @param create  Logical. Try to create the directory if it doesn't exist.
#'
#' @details
#' If `lib_dir` is set to `NULL`, a subdirectory of `tempdir()` will be used.
#' `lib_dir` will be normalized via [normalizePath()].
#'
#' The package cache is under the directory specified by `getOption("apicheck.lib_dir")`,
#' or, if that is unset, in a per-session temporary directory. You should use [set_lib_dir()]
#' to change this rather than setting the option directly. Within this directory, subdirectories
#' are named like `package-version`, e.g. `longurl-0.3.0`. Within these subdirectories are
#' the actual installed libraries. So, `lib_dir` is not appropriate for passing to functions like
#' `library`. To load a library from the cache yourself,
#' do e.g. `library("blah", lib.loc = file.path(get_lib_dir(), "blah-0.1.0"))`.
#'
#' @return `set_lib_dir` invisibly returns the old library location, or `NULL` if none was set in options.
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


#' @return `get_lib_dir` returns the actual library location, whether or not an option has been set.
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



#' @return `clear_lib_dir` returns TRUE if all files and directories could be removed, FALSE otherwise.
#'
#' @family utility functions
#'
#' @rdname set_lib_dir
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clear_lib_dir()
#' }
clear_lib_dir <- function() {
  lib_dir <- get_lib_dir()
  ok <- TRUE
  for (obj in list.files(lib_dir, full.names = TRUE)) {
    if (dir.exists(obj))  ok <- ok && identical(unlink(obj, recursive = TRUE), 0L)
    if (file.exists(obj)) ok <- ok && file.remove(obj)
  }

  return(ok)
}
