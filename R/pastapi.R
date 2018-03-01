

# TODO
# use install.dates
# deal with S4 methods; and test for that.
# github setup


#' @importFrom zeallot %<-%
NULL


#' About the package
#'
#' This is a small package to check when functions were introduced in packages and/or APIs changed.
#' It uses the \code{versions} package to install different versions of a package
#' from \href{https://mran.microsoft.com/}{MRAN}.
#'
#' Packages are cached within a session. To cache packages across sessions, set
#' \code{options('pastapi.lib_dir')} to the file path of a persistent directory.
#'
#' Be aware that functions can take a long time to return as different versions of a package are
#' installed.
#'
#' Also, be aware that namespace loading and unloading can be unreliable. If this happens to you, try
#' restarting your session.
#'
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

#' @param fn Function name as a character string.
#' @param package Package. Alternatively, specify the function name as e.g. \code{"package::function"}.
#' @param version Version as a character string. If omitted, use the version available at \code{date}.
#' @param date Date, as a character string that can be read by \code{\link{as.Date}} e.g. "2016-01-01".
#' @name basic_params_doc
NULL


#' Compare function APIs across package versions
#'
#' \code{api_first_same} reports the first package version where the API of a function was the same as now (or
#' the same as \code{current_fn}). \code{api_same_at} reports whether a specific previous version had the same
#' API as now.
#'
#' @inheritParams basic_params_doc
#' @param quick If \code{TRUE}, do a binary search that assumes API is never same, then different.
#' @inheritParams current_fn_doc
#' @inherit same_api_doc details
#'
#' @return \code{api_first_same} returns a version string.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' api_first_same("read.dta", "foreign")
#' }
api_first_same <- function (fn, package, quick = TRUE,
      current_fn = NULL) {
  if (missing(package)) c(package, fn) %<-% parse_fn(fn)

  force(current_fn)
  vns <- clean_versions(package)
  vns <- vns[seq(nrow(vns), 1),]
  test <- function (version) suppressWarnings(api_same_at(fn, package = package, version = version,
        current_fn = current_fn))
  result <- if (quick) binary_search_versions(vns, test) else Find(test, vns$version)

  return(result)
}


#' Compare function existence across package versions
#'
#' \code{fn_first_exists} reports the first package version where a function exists. \code{fn_exists_at} reports
#' whether a function exists at a specific previous version.
#'
#' @param fn Function name.
#' @param package Package name.
#' @param quick If TRUE, perform a binary search. This assumes that once added, a
#'  function does not go away.
#'
#' @return \code{fn_first_exists} returns a version string.
#' @export
#'
#' @examples
#' \dontrun{
#' fn_first_exists('read.dta', 'foreign')
#' }
fn_first_exists <- function (fn, package, quick = TRUE) {
  if (missing(package)) c(package, fn) %<-% parse_fn(fn)
  vns <- clean_versions(package)
  vns <- vns[seq(nrow(vns), 1),]
  test <- function (version) fn_exists_at(fn, package = package, version = version)
  result <- if (quick) binary_search_versions(vns, test) else Find(test, vns$version)

  return(result)
}


#' @inheritParams basic_params_doc
#' @inheritParams current_fn_doc
#' @details
#' If \code{fn} does not exist at \code{version}, \code{api_same_at} returns \code{FALSE} with a warning.
#' @inherit same_api_doc details
#' @return \code{api_same_at} returns \code{TRUE} or \code{FALSE}.
#' @rdname api_first_same
#' @export
#'
#' @examples
#' \dontrun{
#' api_same_at("huxreg", "huxtable", "2.0.0")
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

  load_version_namespace(package, version, test)
}



#' @inheritParams basic_params_doc
#'
#' @return \code{fn_exists_at} returns \code{TRUE} or \code{FALSE}.
#' @rdname fn_first_exists
#' @export
#'
#' @examples
#' \dontrun{
#' fn_exists_at("read.arff", "foreign", version = "0.8-19")
#' }
fn_exists_at <- function (
        fn,
        package,
        version = get_version_at_date(package, date),
        date = NULL
      ) {
  if (missing(package)) c(package, fn) %<-% parse_fn(fn)
  test <- function (namespace) fn %in% names(namespace)
  load_version_namespace(package, version, test)
}


#' Retrieve a function from a particular package version
#'
#' @inheritParams basic_params_doc
#'
#' @return The function itself.
#' @export
#'
#' @examples
#' \dontrun{
#' get_fn_at("huxreg", "huxtable", "2.0.0")
#' }
get_fn_at <- function (
        fn,
        package,
        version = get_version_at_date(package, date),
        date = NULL
      ) {
  if (missing(package)) c(package, fn) %<-% parse_fn(fn)
  test <- function (namespace) get(fn, namespace)
  load_version_namespace(package, version, test)
}


#' Return the current version of a package at a given date
#'
#' @param package A package as a string.
#' @param date A date.
#'
#' @return A version string.
#' @export
#'
#' @examples
#' \dontrun{
#' get_version_at_date("huxtable", "2017-01-01")
#' }
get_version_at_date <- function (package, date) {
  vns <- clean_versions(package)
  latest <- vns$version[vns$date <= date & vns$available][1]

  return(latest)
}


#' Specify library location
#'
#' This specifies where libraries will be downloaded to,
#' and resets the cache of installed library locations.
#' @param lib_dir Path to a directory.
#'
#' @return The old library location. By default this is a subdirectory of \code{\link{tempdir()}}.
#' @details
#' If \code{lib_dir} does not exist it will be created.
#' @export
#'
#' @examples
#' \dontrun{
#' set_pastapi_lib_dir("~/.pastapi")
#' }
set_pastapi_lib_dir <- function(lib_dir) {
  x <- options('pastapi.lib_dir')
  if (! dir.exists(lib_dir)) dir.create(lib_dir, recursive = TRUE)
  options('pastapi.lib_dir' = lib_dir)
  memoise::forget(cached_install)

  return(x)
}


# loads a package's namespace at a particular version (if necessary installing it)
# then runs the function test with the namespace as an argument, and returns the result
load_version_namespace  <- function (package, version, test) {
  if (isNamespaceLoaded(package)) {
    warning(package, " is already loaded. Attempting to unload.")
    unloadNamespace(package)
  }
  package_dir <- cached_install(package, version)

  namespace <- loadNamespace(package, lib.loc = package_dir, partial = TRUE)
  on.exit(unloadNamespace(package))

  test(namespace)
}


cached_install <- memoise::memoise(
  function (package, version) {
    lib_dir <- getOption('pastapi.lib_dir', tempfile(pattern = "pastapi", tmpdir = normalizePath(tempdir())))
    if (is.null(options('pastapi.lib_dir'))) options(pastapi.lib_dir = lib_dir)
    package_dir <- file.path(lib_dir, paste(package, version, sep = "-"))

    if (! dir.exists(package_dir)) {
      dir.create(package_dir, recursive = TRUE)
      # just shut up already:
      suppressWarnings(
        versions::install.versions(package, versions = version, lib = package_dir, verbose = FALSE, quiet = TRUE)
      )
    }

    return(package_dir)
  }
)


binary_search_versions <- function(vns, test) {
  versions <- vns$version
  i <- 1L
  j <- length(versions)
  if (test(versions[i])) return(versions[i])
  if (! test(versions[j])) return(NULL)
  # maintain that false for i, true for j
  while (TRUE) {
    midpoint <- as.integer(floor((i + j) / 2))
    res <- test(versions[midpoint])
    if (res) {
      if (midpoint <= i + 1) return(versions[midpoint])
      j <- midpoint
    } else {
      if (midpoint >= j - 1) return(versions[j])
      i <- midpoint
    }
  }
}


clean_versions <- memoise::memoise(
  function (package) {
    vns <- versions::available.versions(package)[[package]]
    vns <- vns[vns$available == TRUE,]
    vns$date <- as.Date(vns$date)
    vns <- vns[order(vns$date, decreasing = TRUE),]

    return(vns)
  }
)


parse_fn <- function (fn) {
  if (! grepl("::", fn)) stop("No `package` specified or found in function name")
  strsplit(fn, "::", fixed = TRUE)[[1]]
}
