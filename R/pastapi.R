

# TODO
# deal with S4 methods; and test for that.
# allow CRAN and devtools::install_version; or patch versions to work with CRAN
# fix documentation - probably put search functions together


#' @importFrom zeallot %<-%
NULL


#' Basic details about the package
#'
#' This is a small package to check when functions were introduced in packages and/or APIs changed.
#' It uses the \code{versions} package to install different versions of a package
#' from \href{https://mran.microsoft.com/}{MRAN}.
#'
#' Packages are cached within a session. To cache packages across sessions, use
#' \code{\link{set_lib_dir}} to point to a persistent directory.
#'
#' Be aware that functions can take a long time to return, as different versions of a package are
#' installed and/or loaded.
#'
#' Also, be aware that namespace loading and unloading can be unreliable. If this happens to you, try
#' restarting your session.
#' @name pastapi-package
NULL


#' @param current_fn Current function for comparison. By default, \code{fn} in the current version of
#'   the package (which is assumed to be available in a standard library location). If provided, this
#'   must be an actual function, not a character string. You can use
#'   \code{\link{get_fn_at}} for this.
#' @name current_fn_doc
NULL


#' @details
#' "Same API" is defined by the function arguments, as reported by \code{\link{formals}}, being the same.
#' @name same_api_doc
NULL


#' @details
#' This function downloads and installs multiple versions from MRAN, so it is likely to be slow.
#' @name slow_warning_doc
NULL


#' @param fn Function name as a character string.
#' @param package Package. Alternatively, specify the function name as e.g. \code{"package::function"}.
#' @name basic_params_doc
NULL


#' @param version Version as a character string. If omitted, use the version available at \code{date}.
#' @param date Date, as a character string that can be read by \code{\link{as.Date}} e.g. "2016-01-01".
#' @name version_params_doc
NULL


#' @param date Date, as a character string that can be read by \code{\link{as.Date}} e.g. "2016-01-01".
#' @name version_params_doc
NULL


#' Test if a function's API is unchanged at a given version
#'
#' \code{api_same_at} reports whether a function had the same API at a previous version or date.
#'
#' @inherit basic_params_doc params
#' @inherit version_params_doc params
#' @inherit current_fn_doc params
#'
#' @details
#' If \code{fn} does not exist at \code{version}, \code{api_same_at} returns \code{FALSE} with a warning.
#' @inherit same_api_doc details
#'
#' @return \code{TRUE} or \code{FALSE}.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' api_same_at("clipr::write_clip", version = "0.1.1")
#' # equivalently
#' api_same_at("write_clip", "clipr", "0.1.1")
#' }
api_same_at <- function (fn, package, version = get_version_at_date(package, date), date = NULL,
  current_fn = NULL) {
  if (missing(package)) c(package, fn) %<-% parse_fn(fn)
  if (missing(current_fn) || is.null(current_fn)) {
    cur_namespace <- loadNamespace(package, partial = TRUE)
    current_fn <- get(fn, cur_namespace)
    unloadNamespace(package)
  }
  test <- function (namespace) {
    g <- tryCatch(
      get(fn, namespace),
      error = function (e) {warning(e$message); return(NULL)}
    )
    if (is.null(g)) return(FALSE)
    identical(formals(current_fn), formals(g))
  }

  call_with_namespace(package, version, test)
}



#' Test if a function exists at a given version
#'
#' \code{fn_exists_at} reports whether a function exists at a specific previous version or date.
#'
#' @inherit basic_params_doc params
#' @inherit version_params_doc params
#'
#' @return \code{TRUE} or \code{FALSE}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fn_exists_at("clipr::dr_clipr", version = "0.3.1")
#' # or
#' fn_exists_at("dr_clipr", "clipr", "0.3.1")
#' }
fn_exists_at <- function (
        fn,
        package,
        version = get_version_at_date(package, date),
        date = NULL
      ) {
  if (missing(package)) c(package, fn) %<-% parse_fn(fn)
  test <- function (namespace) fn %in% names(namespace)
  call_with_namespace(package, version, test)
}


#' Retrieve a function from a particular package version
#'
#' @inherit basic_params_doc params
#' @inherit version_params_doc params
#'
#' @return The function itself.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_fn_at("write_clip", "clipr", "0.1.1")
#' }
get_fn_at <- function (
        fn,
        package,
        version = get_version_at_date(package, date),
        date = NULL
      ) {
  if (missing(package)) c(package, fn) %<-% parse_fn(fn)
  test <- function (namespace) get(fn, namespace)
  call_with_namespace(package, version, test)
}


#' Return the current version of a package at a given date
#'
#' @param package A package as a string.
#' @param date A date.
#'
#' @return A version string.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_version_at_date("huxtable", "2017-01-01")
#' }
get_version_at_date <- function (package, date) {
  vns <- mran_versions(package)
  vns <- vns$version[vns$date <= date & vns$available]
  latest <- vns[length(vns)]

  return(latest)
}



#' Loads a package namespace at a particular version and runs an arbitrary function
#'
#' @param package Package name.
#' @param version Version as a character string.
#' @param test A one-argument function. See Details.
#'
#' @details
#' The package is downloaded and installed if necessary, and its namespace is loaded. Then the
#' \code{test(ns)} is called with the namespace object, and its value is returned. On exit, the
#' namespace is unloaded, hopefully leaving your environment clean.
#'
#' @return The value returned by \code{test}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' can_it_expand_urls <- function (namespace) "expand_urls" %in% names(namespace)
#' call_with_namespace("longurl", "0.3.0", test = can_it_expand_urls)
#' }
call_with_namespace  <- function (package, version, test) {
  namespace <- load_version_namespace(package, version)
  on.exit(unloadNamespace(package))
  test(namespace)
}


#' Load the namespace from a version of a package
#'
#' @param package Package name.
#' @param version Version as a character string.
#' @param cache   If FALSE, always try to reinstall the package.
#'
#' @details
#' If the package is not found in the package cache, it will be downloaded and
#' installed there.
#'
#' If the package is already loaded, \code{load_version_namespace} will first attempt
#' to unload it with a warning. This may not always work!
#'
#' Note that the namespace is not attached.
#'
#' @return The namespace object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_version_namespace("clipr", "0.4.0")
#' }
load_version_namespace <- function (package, version, cache = TRUE) {
  if (isNamespaceLoaded(package)) {
    warning(package, " namespace is already loaded. Attempting to unload.")
    unloadNamespace(package)
  }

  force(version)
  lib_dir <- get_lib_dir()
  package_dir <- file.path(lib_dir, paste(package, version, sep = "-"))

  if (! cache && dir.exists(package_dir)) {
    if (! identical(unlink(package_dir, recursive = TRUE), 0L)) stop(
          "Could not delete old package directory '", package_dir, "'")
  }
  if (! cache || ! dir.exists(package_dir)) {
    dir.create(package_dir, recursive = TRUE)
    if (! dir.exists(package_dir)) stop("Could not create ", package_dir)
    tryCatch(
      versions::install.versions(package, versions = version, lib = package_dir, verbose = TRUE),
      warning = function (w) {
        if (grepl("non-zero exit", w$message)) {
          loudly_unlink(package_dir)
          stop("Failed to install version ", version, ", aborting")
        } else {
          warning(w$message)
        }
      },
      error = function (e) {
        loudly_unlink(package_dir)
        stop(e$message)
      })
  }

  namespace <- tryCatch(
    loadNamespace(package, lib.loc = package_dir, partial = TRUE),
    error = function (e) {
      loudly_unlink(package_dir)
      stop("Failed to load the '", package, "' namespace.\n",
        "Maybe something went silently wrong during installation.")
    }
  )

  return(namespace)
}

#' Report versions available on MRAN
#'
#' This is a simple wrapper round \code{\link[versions]{available.versions}}. It
#' returns only packages on MRAN and is ordered by date. Results are cached so as to
#' relieve pressure on the MRAN server.
#'
#' @param package A single package name as a character string.
#'
#' @return A data frame with columns "version", "date" and "available" (always \code{TRUE}).
#'
#' @export
#'
#' @examples
#' mran_versions("clipr")
#'
mran_versions <- memoise::memoise(
  function (package) {
    vns <- versions::available.versions(package)[[package]]
    vns <- vns[vns$available == TRUE,]
    vns$date <- as.Date(vns$date)
    vns <- vns[order(vns$date),]

    return(vns)
  }
)


loudly_unlink <- function (dir) {
  if (dir.exists(dir) && ! identical(unlink(dir, recursive = TRUE), 0L)) stop(
        "Could not unlink package dir ", dir, " after failed installation. ",
        "Please delete the directory yourself or run clear_package_cache() to delete all directories")

  invisible(NULL)
}


parse_fn <- function (fn) {
  if (! grepl("::", fn)) stop("No `package` specified or found in function name")
  strsplit(fn, "::", fixed = TRUE)[[1]]
}
