
# functions for day-to-day use, but not nec. with "pretty" outpu

# TODO
# deal with S4 methods; and test for that.
# tests: maybe rather than pre-installed (OS X) versions, have pre-installed source
# files and then mock install.versions
# add function to check incremental burden of a dependency?
# BUG: compare_versions fails silently when installed library is used (namespace unloading?)
# BUG: compare_versions seems to overreport API changes, e.g. try `reprex`
#   - One underlying reason is that `x <- loadNamespace(..., partial = TRUE)``
#     can result in no exports from `getNamespaceExports(x)`; but not if the namespace has
#     previously been loaded with partial = FALSE. And this applies even if the namespaces
#     are separate objects.
#   - Be aware especially that if the user has previously loaded a package (even indirectly
#     via a dependency) then getNamespaceExports will return stuff from partial = TRUE; if
#     not, not.
#   - Relevant example:
# > ns <- loadNamespace("clipr", partial = TRUE)
# > getNamespaceExports(ns)
# character(0)
# > ns2 <- loadNamespace("clipr", partial = FALSE)
# > getNamespaceExports(ns)
# character(0)
# > getNamespaceExports(ns2)
# [1] "clipr_available" "write_clip"      "clear_clip"      "read_clip_tbl"   "dr_clipr"
# [6] "read_clip"
# > ns3 <- loadNamespace("clipr", partial = FALSE)
# > getNamespaceExports(ns3)
# [1] "clipr_available" "write_clip"      "clear_clip"      "read_clip_tbl"   "dr_clipr"
# [6] "read_clip"
# > identical(ns, ns3)
# [1] FALSE
# > identical(ns2, ns3)
# [1] TRUE
#
# Tests for base package rcheology fakery.
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


#' @param fun Function name as a character string. `fun` can be an S3 method; S4 methods aren't yet supported.
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
    current_fun <- if (is_core_package(package)) {
      fun_at(fun, version = as.character(getRversion()), package = package, allow_core = TRUE)
    } else {
      get_fun_in_ns(fun, get_current_ns(package))
    }
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


#' Test if a function exists at a given version
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


#' Retrieve a function from a particular package version
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

  if (is_core_package(package)) {
    rch <- get_rcheology_rows(name = fun, package = package) # memoised for speed
    rch <- rch[rch$Rversion == version,]
    if (nrow(rch) == 0L) stop(FUNCTION_NOT_FOUND)
    stopifnot(nrow(rch) == 1L)
    fun_args <- rch$args
    if (is.na(fun_args)) stop("Could not determine arguments of `", fun, "` in rcheology database of core R functions")
    fake_fun <- paste("function", fun_args, "NULL")
    fake_fun <- eval(parse(text = fake_fun))
    return (fake_fun)
  }

  test <- function (namespace) get_fun_in_ns(fun, namespace)
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
  assert_not_core(package)

  on.exit(unloadNamespace(package))
  package_dir <- cached_install(package, version, return = "path", quiet = quiet, ...)
  # double brackets stop help looking for "fun" literally
  utils::help((fun), package = (package), lib.loc = package_dir, help_type = "text")
}
