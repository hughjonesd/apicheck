

# TODO
# debug parallel
# deal with S4 methods; and test for that.
# tests: maybe rather than pre-installed (OS X) versions, have pre-installed source
# files and then mock install.versions
# rename to apicheck?
# add function to check incremental burden of a dependency?



#' @importFrom zeallot %<-%
NULL


#' Basic details about the package
#'
#' This is a small package to check when functions were introduced in packages and/or APIs changed.
#' It automatically installs different versions of a package in a separate directory and loads them
#' without attaching them.
#'
#' Packages are cached within a session. To cache packages across sessions, use
#' \code{\link{set_lib_dir}} to point to a persistent directory.
#'
#' By default, \code{apicheck} uses the \code{remotes}
#' package to install source versions from CRAN. Alternatively, it can use the \code{versions} package to install different versions of a package
#' from \href{https://mran.microsoft.com/}{MRAN}. To do this set \code{options(apicheck.use_cran = FALSE)}.
#'
#' Be aware that functions can take a long time to return, as different versions of a package are
#' installed and/or loaded.
#'
#' Also, be aware that namespace loading and unloading can be unreliable. If this happens to you, try
#' restarting your session.
#'
#' @section Warning:
#' Do not try to use \code{apicheck} on itself. This will lead to fiery elephants in the sky.
#'
#' @name apicheck-package
NULL


#' @details
#' "Same API" is defined by the function arguments, as reported by \code{\link{formals}}, being the same.
#' @name same_api_doc
NULL


#' @section Speed:
#' This function may download and install multiple versions from MRAN, so it is likely to be slow
#' when first used (and even afterwards if library loading is slow). Using \code{search = "parallel"}
#' may help, but not if the network is the bottleneck.
#' @name slow_warning_doc
NULL



#' @param fn Function name as a character string.
#' @param version Version as a character string. If omitted, use the version available at \code{date}.
#' @param package Package. Alternatively, specify the function name as e.g. \code{"package::function"}.
#' @param date Date, as a character string that can be read by \code{\link{as.Date}} e.g. "2016-01-01".
#' @param current_fn Current function for comparison. By default, \code{fn} in the current version of
#'   the package (which is assumed to be available in a standard library location). If provided, this
#'   must be an actual function, not a character string. You can use
#'   \code{\link{get_fn_at}} for this.
#' @param test    A one-argument function. See Details.
#' @param current_fn Current function
#' @param quiet Logical. Try to minimize output from package installation. (Some output comes from \code{R CMD INSTALL} and may be unavoidable.)
#' @param ... Arguments passed to \code{\link[versions]{install.versions}} or
#'   \code{\link[remotes]{install_version}}, and thence to \code{\link{install.packages}}.
#' @name params_doc
NULL


#' @param version Version as a character string.
#' @name version_nodate_params_doc
NULL


#' @param package Package name.
#' @name package_nofn_params_doc
NULL


#' Test if a function's API is unchanged at a given version
#'
#' \code{api_same_at} reports whether a function had the same API at a previous version or date.
#'
#' @inherit params_doc params
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
api_same_at <- function (
        fn,
        version    = get_version_at_date(package, date),
        package,
        date       = NULL,
        quiet      = TRUE,
        current_fn = NULL,
        ...
      ) {
  if (missing(package)) c(package, fn) %<-% parse_fn(fn)
  if (missing(current_fn) || is.null(current_fn)) {
    cur_namespace <- try(loadNamespace(package, partial = TRUE), silent = TRUE)
    if (class(cur_namespace) == "try-error") stop("Couldn't load current version of package.\n",
          "Do you have it installed? If not run `install.packages('", package, "')`.\n",
          "Or, use `current_fn = get_fn_at(fn, package, version)` to compare to a version\n",
          " without doing a full install.")
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

  call_with_namespace(package, version, test, quiet = quiet, ...)
}


#' Test if a function exists at a given version
#'
#' \code{fn_exists_at} reports whether a function exists at a specific previous version or date.
#'
#' @inherit params_doc params
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
        version = get_version_at_date(package, date),
        package,
        date    = NULL,
        quiet   = TRUE,
        ...
      ) {
  if (missing(package)) c(package, fn) %<-% parse_fn(fn)
  test <- function (namespace) fn %in% names(namespace)
  call_with_namespace(package, version, test, quiet = quiet, ...)
}


#' Retrieve a function from a particular package version
#'
#' @inherit params_doc params
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
        version = get_version_at_date(package, date),
        package,
        date    = NULL,
        quiet   = TRUE,
        ...
      ) {
  if (missing(package)) c(package, fn) %<-% parse_fn(fn)
  test <- function (namespace) get(fn, namespace)
  call_with_namespace(package, version, test, quiet = quiet, ...)
}


#' Get help for a function at a package version
#'
#' @inherit params_doc params
#'
#' @return The help object (text format only).
#'
#' @seealso \code{\link{help}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_help_at("clipr::write_clip", "0.1.1")
#' get_help_at("clipr::write_clip", "0.2.0")
#' }
get_help_at <- function (
        fn,
        version = get_version_at_date(package, date),
        package,
        date    = NULL,
        quiet   = TRUE,
        ...
) {
  if (missing(package)) c(package, fn) %<-% parse_fn(fn)
  on.exit(unloadNamespace(package))
  package_dir <- cached_install(package, version, return = "path", quiet = quiet, ...)
  utils::help((fn), package = (package), lib.loc = package_dir, help_type = "text")
}

#' Loads a package namespace at a particular version and runs an arbitrary function
#'
#' @inherit package_nofn_params_doc params
#' @inherit version_nodate_params_doc params
#' @inherit params_doc params
#' @param test    A one-argument function. See Details.
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
call_with_namespace  <- function (
        package,
        version,
        test,
        quiet = TRUE,
        ...
      ) {
  namespace <- cached_install(package, version, return = "namespace", quiet = quiet, ...)
  on.exit(unloadNamespace(package))
  test(namespace)
}


#' Return a package version's location or namespace, possibly installing it
#'
#' @inherit package_nofn_params_doc params
#' @inherit version_nodate_params_doc params
#' @inherit params_doc params
#' @param return  Return the file "path" to the installed package, or the "namespace" object?
#' @param cache   If \code{FALSE}, always try to reinstall the package.
#' @param partial Default \code{TRUE}. Passed to \code{\link{loadNamespace}}.
#'
#' @details
#' If the package is not found in the package cache, it will be downloaded and
#' installed there.
#'
#' If the package is already loaded, \code{cached_install} will first attempt
#' to unload it with a warning. This may not always work!
#'
#' Note that the namespace is not attached. Partial loading is faster and safer when
#' you are (un)loading multiple versions, but does not export functions etc.
#'
#' @return The namespace object or directory where the package is installed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cached_install("clipr", "0.4.0")
#' }
cached_install <- function (
        package,
        version,
        return  = c("namespace", "path"),
        cache   = TRUE,
        quiet   = TRUE,
        partial = TRUE,
        ...
      ) {
  ret <- match.arg(return)
  if (isNamespaceLoaded(package)) {
    warning(package, " namespace is already loaded. Attempting to unload.")
    unloadNamespace(package)
  }

  force(version)
  lib_dir <- get_lib_dir()
  package_dir <- file.path(lib_dir, paste(package, version, sep = "-"))
  if (! cache) loudly_unlink(package_dir, paste0("Could not delete old package directory '", package_dir, "'"))

  if (! cache || ! dir.exists(package_dir)) {
    dir.create(package_dir, recursive = TRUE)
    if (! dir.exists(package_dir)) stop("Could not create ", package_dir)
    # install.packages spews to stderr, but not via
    # message. So we can't use tryCatch for messages, have to use capture.output. This gets message() output too.
    # RStudio cat()s warnings of install.packages; so they aren't caught in tryCatch. The solution is to
    # cat everything in tryCatch and then to capture.output twice.
    here <- environment()
    capture_all <- function (expr) {
      msg <- utils::capture.output(out <- utils::capture.output(eval(substitute(expr, here))), type = "message")
      return(list(msg = msg, out = out))
    }
    output <- capture_all(tryCatch({
      if (isTRUE(getOption('apicheck.use_cran', TRUE))) {
        withr::with_libpaths(package_dir,
          remotes::install_version(package, version, lib = package_dir, type = "source", quiet = quiet, ...)
        )
      } else {
        versions::install.versions(package, versions = version, lib = package_dir,  ...)
      }
    },
      warning = function (w) {
        if (grepl("non-zero exit", w$message)) {
          loudly_unlink(package_dir)
          stop("Failed to install version ", version)
        } else {
          cat(w$message)
        }
      },
      error = function (e) {
        loudly_unlink(package_dir)
        stop(e$message, call. = FALSE)
      }))
    if (! quiet) {
      message(output$msg)
      cat(output$out)
    }
  }

  tryCatch(
    namespace <- loadNamespace(package, lib.loc = package_dir, partial = partial),
    error = function (e) {
      loudly_unlink(package_dir)
      stop(
              "Failed to load the namespace of '", package, "' version '", version, "'.\n",
              "Maybe something went silently wrong during installation.",
              if (quiet && exists("output")) paste0(
                "\nOutput from install.packages is below:\n==========\nMessages:\n", output$msg,
                "\n==========\nOutput:\n", output$out, "\n==========\n"
              ),
              call. = FALSE
            )
    }
  )

  res <- if (ret == "namespace") namespace else package_dir
  return(res)
}


#' Report available versions
#'
#' This returns packages ordered by date, using either \href{https://mran.microsoft.com}{MRAN} or
#' \href{http://crandb.r-pkg.org}{metacran}.
#' Results are cached so as to
#' relieve pressure on the server. If \code{options("apicheck.use_cran") == FALSE},
#' then only versions available on MRAN (i.e. after 2014-09-17) will be returned;
#' otherwise older versions will be returned too.
#'
#' @inherit package_nofn_params_doc params
#'
#' @section Speed:
#' In my limited experience, metacran is much faster. YMMV.
#'
#' @return A data frame with columns "version" and "date".
#'
#' @export
#'
#' @examples
#' \dontrun{
#' available_versions("clipr")
#' }
#'
available_versions <- memoise::memoise(
  function (package) {
    vns_df <- if (isTRUE(getOption("apicheck.use_cran", TRUE))) av_metacran(package) else av_mran(package)
    vns_df$date <- as.Date(vns_df$date)
    vns_df <- vns_df[order(vns_df$date), ]
    if (! isTRUE(getOption("apicheck.use_cran", TRUE))) vns_df <- vns_df[vns_df$Date >= "2014-09-17", ]

    return(vns_df)
  }
)
av_mran <- function (package) {
  vns_df <- versions::available.versions(package)[[package]]
  vns_df$available <- NULL

  return(vns_df)
}

# code snatched from https://github.com/metacran/crandb
av_metacran <- function (package) {
  res <- httr::GET(paste0("http://crandb.r-pkg.org/", package, "/all"))
  res <- httr::content(res, as = "text", encoding = "UTF-8")
  res <- jsonlite::fromJSON(res)$timeline
  vns_df <- data.frame(version = names(res), date = as.character(res), stringsAsFactors = FALSE)

  return(vns_df)
}


#' Return the current version of a package at a given date
#'
#' @inherit params_doc params
#' @inherit package_nofn_params_doc params
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
  vns_df <- available_versions(package)
  vns_df <- vns_df$version[vns_df$date <= date]
  latest <- vns_df[length(vns_df)]

  return(latest)
}


loudly_unlink <- function (dir, error = paste0("Could not unlink package dir ", dir,
        " after failed installation.\n",
        "Please delete the directory yourself or run clear_package_cache()",
        "to delete all directories")) {
  if (dir.exists(dir) && ! identical(unlink(dir, recursive = TRUE), 0L)) stop(error)

  invisible(NULL)
}


parse_fn <- function (fn) {
  if (! grepl("::", fn)) stop("No `package` specified or found in function name")
  strsplit(fn, "::", fixed = TRUE)[[1]]
}
