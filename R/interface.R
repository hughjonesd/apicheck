
# functions for day-to-day use, but not nec. with "pretty" output


#' @importFrom zeallot %<-%
NULL


#' apicheck: check function APIs in different versions of packages
#'
#' This is a small package to check when functions were introduced and/or APIs changed in packages.
#' It automatically installs different versions of a package in a separate directory and loads them
#' without attaching them.
#'
#' Packages are cached within a session. To cache packages across sessions, use
#' [set_lib_dir()] to point to a persistent directory.
#'
#' By default, `apicheck`` uses the `remotes`
#' package to install source versions from CRAN. Alternatively, it can use the `versions`` package to install different versions of a package
#' from \href{MRAN}{https://mran.microsoft.com/}. To do this set `options(apicheck.use_mran = TRUE)`.
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


#' @param fun Function name as a character string. `fun` can be an S3 method; S4 methods aren't yet supported.
#' @param version Version as a character string. If omitted, use the version available at `date`.
#' @param package Package. Alternatively, specify the function name as e.g. `"package::function"`.
#' @param date Date, as a character string that can be read by [as.Date()] e.g. "2016-01-01".
#' @param current_fun Current function for comparison. By default, `fun` in the current version of
#'   the package (which is assumed to be already installed). This
#'   must be an actual function, not the name of one: use [fun_at()].
#' @param test  A one-argument function. See Details.
#' @param quiet Logical. Try to minimize output from package installation. (Some output comes
#'   from `R CMD INSTALL` and may be unavoidable.)
#' @param ... Arguments passed to [versions::install.versions()] or
#'   [remotes::install_version()], and thence to [install.packages()]. `Ncpus` may be useful.
#' @name params_doc
NULL


#' @param version Version as a character string.
#' @name version_nodate_params_doc
NULL


#' @param package Package name.
#' @name package_nofun_params_doc
NULL


#' Test if a function's API is unchanged
#'
#' `api_same_at` reports whether a function had the same API at a previous version or date.
#'
#' @inherit params_doc params
#'
#' @details
#' If `fun` is not exported at `version`, `api_same_at` returns `FALSE` with a warning.
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
  if (is.null(current_fun)) {
    current_fun <- get_fun_in_ns(fun, get_current_ns(package))
  }

  g <- tryCatch(
    fun_at(fun, version = version, package = package, allow_core = TRUE, quiet = quiet, ...),
    error = function (e) {
      if (grepl(FUNCTION_NOT_FOUND, e$message, fixed = TRUE)) {
        warning(e$mesage)
        return(NULL)
      } else {
        stop(e$message)
      }
    }
  )

  if (is.null(g)) return(FALSE)
  return(is_api_same(current_fun, g))
}


#' Test if a function exists at a given package version
#'
#' `fun_exists_at` reports whether a function exists (i.e. is exported) from a package at a specific previous version
#' or date.
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
  g <- tryCatch(
    fun_at(fun, version = version, package = package, allow_core = TRUE, quiet = quiet, ...),
    error = function (e) {
      if (grepl(FUNCTION_NOT_FOUND, e$message, fixed = TRUE)) {
        return(NULL)
      } else {
        stop(e$message)
      }
    }
  )

  return(! is.null(g))
}


#' Retrieve a function from a package version
#'
#' @inherit params_doc params
#' @param allow_core See below.
#'
#' @details
#' By default, you cannot get functions from previous versions of R core packages, which are not available
#' on CRAN/MRAN. If `allow_core` is `TRUE`, then a function will be returned from the
#' [rcheology](https://github.com/hughjonesd/rcheology) dataset, with a null body but the same formal
#' arguments as the historical function.
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
  date       = NULL,
  quiet      = TRUE,
  allow_core = FALSE,
  ...
) {
  if (missing(package)) c(package, fun) %<-% parse_fun(fun)
  if (! allow_core) assert_not_core(package)

  fun_list <- funs_at(list(fun), version, package, quiet, ...)

  return(fun_list[[1]])
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
  assert_not_core(package)

  on.exit(unload_noncore_namespace(package))
  package_dir <- cached_install(package, version, return = "path", quiet = quiet, ...)
  # double brackets stop help looking for "fun" literally
  utils::help( (fun), package = (package), lib.loc = package_dir, help_type = "text")
}
