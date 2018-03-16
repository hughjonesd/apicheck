

# TODO
# deal with S4 methods; and test for that.
# tests: maybe rather than pre-installed (OS X) versions, have pre-installed source
# files and then mock install.versions
# add function to check incremental burden of a dependency?
# BUG: compare_versions fails silently when installed library is used (namespace unloading?)
# BUG: compare_versions seems to overreport API changes, e.g. try `reprex`
# parallelize binary and other methods
#  - Should bring big speedups
# if we have partial, can we load methods? CHECK.
# Make versions a Suggests
# Clean separate API: installing stuff in the package cache; querying it. Always two separate operations.
# Clean unloading and reloading of current packages; always leave the computer in the state it was in before.


#' @importFrom zeallot %<-%
NULL


#' Basic details about the package
#'
#' This is a small package to check when functions were introduced in packages and/or APIs changed.
#' It automatically installs different versions of a package in a separate directory and loads them
#' without attaching them.
#'
#' Packages are cached within a session. To cache packages across sessions, use
#' [set_lib_dir()] to point to a persistent directory.
#'
#' By default, `apicheck`` uses the `remotes`
#' package to install source versions from CRAN. Alternatively, it can use the `versions`` package to install different versions of a package
#' from [MRAN]{https://mran.microsoft.com/}. To do this set `options(apicheck.use_mran = TRUE)`.
#'
#' Be aware that functions can take a long time to return, as different versions of a package are
#' installed and/or loaded.
#'
#' Also, be aware that namespace loading and unloading can be unreliable. If this happens to you, try
#' restarting your session.
#'
#' @section Warning:
#' Do not try to use `apicheck` on itself. This will lead to fiery elephants in the sky.
#'
#' @name apicheck-package
NULL


#' @details
#' "Same API" means having the same function arguments, as reported by [formals()].
#' @name same_api_doc
NULL


#' @section Speed:
#' This function may download and install multiple versions from MRAN, so it is likely to be slow
#' when first used (and even afterwards if library loading is slow). Using `search = "parallel"`
#' may help, but not if the network is the bottleneck: see
#' (here)[https://hughjonesd.github.io/apicheck/performance2.html] for details.
#' @name slow_warning_doc
NULL


#' @param fun Function name as a character string.
#' @param version Version as a character string. If omitted, use the version available at `date`.
#' @param package Package. Alternatively, specify the function name as e.g. `"package::function"`.
#' @param date Date, as a character string that can be read by [as.Date()] e.g. "2016-01-01".
#' @param current_fun Current function for comparison. By default, `fun` in the current version of
#'   the package (which is assumed to be available in a standard library location). If provided, this
#'   must be an actual function, not a character string. You can use
#'   [fun_at()] for this.
#' @param test    A one-argument function. See Details.
#' @param current_fun Current function
#' @param quiet Logical. Try to minimize output from package installation. (Some output comes from `R CMD INSTALL` and may be unavoidable.)
#' @param ... Arguments passed to [versions::install.versions()] or
#'   [remotes::install_version()], and thence to [install.packages()].
#' @name params_doc
NULL


#' @param version Version as a character string.
#' @name version_nodate_params_doc
NULL


#' @param package Package name.
#' @name package_nofun_params_doc
NULL


#' Test if a function's API is unchanged at a given version
#'
#' `api_same_at` reports whether a function had the same API at a previous version or date.
#'
#' @inherit params_doc params
#'
#' @details
#' If `fun` does not exist at `version`, `api_same_at` returns `FALSE` with a warning.
#' @inherit same_api_doc details
#'
#' @return `TRUE` or `FALSE`.
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
        fun,
        version    = version_at_date(package, date),
        package,
        date       = NULL,
        quiet      = TRUE,
        current_fun = NULL,
        ...
      ) {
  if (missing(package)) c(package, fun) %<-% parse_fun(fun)
  if (missing(current_fun) || is.null(current_fun)) {
    cur_ns <- get_current_ns(package)
    current_fun <- get(fun, cur_ns)
  }
  test <- function (namespace) {
    g <- tryCatch(
      get(fun, namespace),
      error = function (e) {warning(e$message); return(NULL)}
    )
    if (is.null(g)) return(FALSE)
    identical(formals(current_fun), formals(g))
  }

  call_with_namespace(package, version, test, quiet = quiet, ...)
}


#' Test if a function exists at a given version
#'
#' `fun_exists_at` reports whether a function exists at a specific previous version or date.
#'
#' @inherit params_doc params
#'
#' @return `TRUE` or `FALSE`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fun_exists_at("clipr::dr_clipr", version = "0.3.1")
#' # or
#' fun_exists_at("dr_clipr", "clipr", "0.3.1")
#' }
fun_exists_at <- function (
        fun,
        version = version_at_date(package, date),
        package,
        date    = NULL,
        quiet   = TRUE,
        ...
      ) {
  if (missing(package)) c(package, fun) %<-% parse_fun(fun)
  test <- function (namespace) fun %in% names(namespace)
  call_with_namespace(package, version, test, quiet = quiet, ...)
}


#' Compare versions of a package and report changed functions and APIs
#'

#' @param version  First version to compare. If `NULL`, use the previous available version.
#' @param version2 Second version to compare. If `NULL`, use the current version as installed.
#' @inherit package_nofun_params_doc params
#' @inherit params_doc params
#'
#' @return A data frame reporting functions that have been "Added", "Removed" or had "API changed",
#'   and details of function arguments.
#' @export
#'
#' @examples
#' \dontrun{
#' compare_versions("clipr", "0.2.1", "0.3.0")
#' }
compare_versions <- function (
        package,
        version = previous_version(package),
        version2 = NULL,
        quiet    = TRUE,
        ...
      ) {
  # if NULL, version2 is latest and version is previous
  if (missing(version2) || is.null(version2)) {
    cur_ns <- get_current_ns(package)
    version2 <- current_version(package)
  } else {
    cur_ns <- cached_install(package, version2, return = "namespace", partial = FALSE)
  }

  test <- function (ns) {
    versions_report(ns, cur_ns, version, version2)
  }

  call_with_namespace(package, version, test, quiet = quiet, partial = FALSE, ...)
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
#' fun_at("write_clip", "clipr", "0.1.1")
#' }
fun_at <- function (
        fun,
        version = version_at_date(package, date),
        package,
        date    = NULL,
        quiet   = TRUE,
        ...
      ) {
  if (missing(package)) c(package, fun) %<-% parse_fun(fun)
  test <- function (namespace) get(fun, namespace)
  call_with_namespace(package, version, test, quiet = quiet, ...)
}


#' Get help for a function at a package version
#'
#' @inherit params_doc params
#'
#' @return The help object (text format only).
#'
#' @seealso [help()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' help_at("clipr::write_clip", "0.1.1")
#' help_at("clipr::write_clip", "0.2.0")
#' }
help_at <- function (
        fun,
        version = version_at_date(package, date),
        package,
        date    = NULL,
        quiet   = TRUE,
        ...
) {
  if (missing(package)) c(package, fun) %<-% parse_fun(fun)
  on.exit(unloadNamespace(package))
  package_dir <- cached_install(package, version, return = "path", quiet = quiet, ...)
  # double brackets stop help looking for "fun" literally
  utils::help((fun), package = (package), lib.loc = package_dir, help_type = "text")
}

#' Load a package namespace at a particular version and run an arbitrary function
#'
#' @inherit package_nofun_params_doc params
#' @inherit version_nodate_params_doc params
#' @inherit params_doc params
#' @param test    A one-argument function. See Details.
#'
#' @details
#' The package is downloaded and installed if necessary, and its namespace is loaded. Then the
#' `test(ns)` is called with the namespace object, and its value is returned. On exit, the
#' namespace is unloaded, hopefully leaving your environment clean.
#'
#' @return The value returned by `test`.
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
#' @inherit package_nofun_params_doc params
#' @inherit version_nodate_params_doc params
#' @inherit params_doc params
#' @param return  Return the file "path" to the installed package, or the "namespace" object?
#' @param cache   If `FALSE`, always reinstall the package.
#' @param partial Default `TRUE`. Passed to [loadNamespace()].
#'
#' @details
#' If the package is not found in the package cache, it will be downloaded and
#' installed there.
#'
#' If the package is already loaded, `cached_install` will first attempt
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
      if (mran_selected()) {
        assert_package("versions")
        versions::install.versions(package, versions = version, lib = package_dir,  ...)
      } else {
        withr::with_libpaths(package_dir,
          remotes::install_version(package, version, lib = package_dir, type = "source", quiet = quiet, ...)
        )
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


get_current_ns <- function (package) {
  ns <- try(loadNamespace(package, partial = TRUE), silent = TRUE)
  if (class(ns) == "try-error") stop("Couldn't load current version of package.\n",
    "Do you have it installed? If not run `install.packages('", package, "')`.\n",
    "Or, use `current_fun = fun_at(fun, package, version)` to compare to a version\n",
    " without doing a full install.")
  unloadNamespace(package)

  return(ns)
}


loudly_unlink <- function (dir, error = paste0("Could not unlink package dir ", dir,
        " after failed installation.\n",
        "Please delete the directory yourself or run `clear_lib_dir()`",
        "to delete all directories")) {
  if (dir.exists(dir) && ! identical(unlink(dir, recursive = TRUE), 0L)) stop(error)

  invisible(NULL)
}


mran_selected <- function () isTRUE(getOption("apicheck.use_mran", FALSE))


assert_package <- function (package) {
  if (! requireNamespace(package, quietly = TRUE)) {
    stop("Could not load the `", package, "` library.\n", "Try `install.packages(", package, ")`.")
  }
}


parse_fun <- function (fun) {
  if (! grepl("::", fun)) stop("No `package` specified or found in function name")
  strsplit(fun, "::", fixed = TRUE)[[1]]
}
