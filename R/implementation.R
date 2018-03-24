
# functions for advanced use or private functions related to loading namespaces

#' @importFrom remotes install_version
NULL

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
        quiet      = TRUE,
        ...
      ) {
  namespace <- cached_install(package, version, return = "namespace", quiet = quiet, ...)
  on.exit(unload_noncore_namespace(package))
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
        return     = c("namespace", "path"),
        cache      = TRUE,
        quiet      = TRUE,
        partial    = TRUE,
        ...
      ) {
  ret <- match.arg(return)
  assert_not_core(package)

  if (isNamespaceLoaded(package)) {
    warning(package, " namespace is already loaded. Trying to unload; will try to reload later.")
    unload_noncore_namespace(package)
    on.exit(loadNamespace(package))
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
          install_version(package, version, lib = package_dir, type = "source", quiet = quiet, ...)
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
                "\nOutput from install.packages is below:",
                "\n==========\nMessages:\n", output$msg,
                "\n==========\nOutput:\n", output$out, "\n==========\n"
              ),
              call. = FALSE
            )
    }
  )

  res <- if (ret == "namespace") namespace else package_dir
  return(res)
}


funs_at <- function (
  funs,
  version,
  package,
  quiet      = TRUE,
  ...
) {
  res <- if (is_core_package(package)) {
    lapply(funs, get_stub_fun_in_core, package, version)
  } else {
    test <- function (namespace) {
      lapply(funs, get_fun_in_ns, namespace)
    }
    call_with_namespace(package, version, test, quiet = quiet, ...)
  }

  return(res)
}


loudly_unlink <- function (dir, error = paste0("Could not unlink package dir ", dir,
        " after failed installation.\n",
        "Please delete the directory yourself or run `clear_lib_dir()`",
        "to delete all directories")) {
  if (dir.exists(dir) && ! identical(unlink(dir, recursive = TRUE), 0L)) stop(error)

  invisible(NULL)
}
