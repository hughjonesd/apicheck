
# For high level interface with formatted output

#' @import purrr
#' @import dplyr
NULL

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
  version  = previous_version(package),
  version2 = NULL,
  quiet    = TRUE,
  ...
) {
  if (missing(version2) || is.null(version2)) {
    ns2 <- get_current_ns(package)
    version2 <- current_version(package)
  } else {
    ns2 <- cached_install(package, version2, return = "namespace", partial = FALSE)
  }

  test <- function (ns) versions_report(ns, ns2, version, version2)

  call_with_namespace(package, version, test, quiet = quiet, partial = FALSE, ...)
}


versions_report <- function (ns1, ns2, v1, v2) {
  suffs <- c("_1", "_2")
  get_funs <- function (ns) Filter(function (x) is.function(get(x, ns)), getNamespaceExports(ns))
  objs1 <- get_funs(ns1)
  objs2 <- get_funs(ns2)

  report1 <- tibble::tibble(f = objs1, f1 = objs1)
  report2 <- tibble::tibble(f = objs2, f2 = objs2)
  report <- full_join(report1, report2, all = TRUE, suffix = suffs, by = "f")
  report <- report[, 2:3]
  names(report) <- paste0("function", suffs)
  report$change <- NA_character_
  report$api_1  <- NA_character_
  report$api_2  <- NA_character_
  report$change[is.na(report$function_1)] <- "Added"
  report$change[is.na(report$function_2)] <- "Removed"
  both_there <- is.na(report$change)

  iwalk(report[both_there, 1, drop = TRUE], function (x, idx) {
    f1 <- get_fun_in_ns(x, ns1)
    f2 <- get_fun_in_ns(x, ns2)
    if (! is_api_same(f1, f2)) {
      report[both_there, ][idx, "change"] <- "API changed"
      report[both_there, ][idx, "api_1"] <- as.list(formals(f1))
      report[both_there, ][idx, "api_2"] <- as.list(formals(f2))
    }
  })

  report <- report[! is.na(report$change), ]
  class(report) <- c("versions_report", class(report))
  return(report)
}


#' Report on backwards compatibility of a source package
#'
#' @param path Path to the root of an R package.
#' @param include Packages to include. By default, all are included except core packages and the package at
#'   `path` itself.
#' @param exclude Packages to exclude from checking. Overrides `include`.
#' @param cutoff_date Don't check versions before this date (default: 2 years ago). Set to `NULL` for no cutoff.
#' @param max_n_versions Check at most this number of previous versions. Set to `NULL` for no limit.
#' @param parallel Run in parallel.
#' @param progress Show a progress bar.
#' @param ... Arguments passed to [call_with_namespace()].
#'
#' @details
#' This function lists the external function calls from a source package using [pkgapi::map_package()].
#' It then checks back-compatibility of each call us
#' Only function calls which are qualified with `::` will be detected, so e.g. `purrr::quietly`
#' will be checked, but `quietly` on its own won't be.
#'
#' @inheritSection parallel_doc Parallelism
#'
#' @return A data frame with three rows:
#' * `package` for the external package
#' * `version` for the latest version which caused problems or `NA` if there were no problems
#' * `funs`    a list-column of function names where the API changed
#' @export
#'
#' @examples
#' \dontrun{
#' package_report(".")
#' # to include base packages also:
#' package_report(".", exclude = character(0))
#' }
package_report <- function (
        path           = ".",
        include,
        exclude        = core_packages(),
        cutoff_date    = Sys.Date() - 2 * 365,
        max_n_versions = NULL,
        parallel       = FALSE,
        progress       = ! parallel,
        ...
      ) {
  callees <- package_callees(path, include, exclude)
  packages <- unique(callees$package)
  if (progress) pb <- txtProgressBar(style = 3)
  c(lapply_fun, cl) %<-% make_lapply_fun(parallel = parallel, max_ncores = length(packages))
  results <- lapply_fun(packages, function (package) {
    fun_names <- callees$fun[callees$package == package]
    current_funs <- lapply(fun_names,
            possibly(get_fun_in_ns, otherwise = NA, quiet = FALSE),
            get_current_ns(package)
          )
    bad <- is.na(current_funs)
    current_funs <- current_funs[! bad]
    fun_names <- fun_names[! bad]
    versions <- available_versions(package)
    versions <- arrange(versions, desc(date))
    if (! is.null(cutoff_date)) versions <- filter(versions, date >= cutoff_date)
    if (! is.null(max_n_versions)) versions <- slice(versions, seq_len(max_n_versions))

    problem_funs <- character(0)
    problem_version <- NA_character_
    for (vn in seq_len(nrow(versions))) {
      version <- versions$version[vn]
      funs <- funs_at(fun_names, version, package)
      apis_same <- map2_lgl(funs, current_funs, is_api_same)
      problem_funs <- fun_names[! apis_same]
      if (length(problem_funs) > 0) {
        problem_version <- version
        break
      }
    }
    if (progress) setTxtProgressBar(pb, getTxtProgressBar(pb) + 1/length(packages))
    return(list(version = problem_version, funs = problem_funs))
  })
  names(results) <- packages

  if (parallel) maybe_stop_cluster(cl)
  report <- tibble::tibble(
    package = packages,
    version = map_chr(results, "version"),
    funs    = map(results, "funs")
  )

  return(report)
}


package_callees <- function(path, include, exclude = character(0)) {
  assert_package("pkgapi")

  fun_map <- pkgapi::map_package(path)
  callees <- fun_map$calls %>% select(to)
  callees[, c("package", "fun")] <- parse_fun(callees$to, single = FALSE)
  if (! missing(include)) callees <- filter(callees, package %in% include)
  itself <- desc::desc_get("Package", path)
  # "NA" is dynamically created; "" is local:
  # "NA" can also be an external function that is imported :-(
  exclude <- c("", itself, exclude, "NA")
  callees <- filter(callees, ! package %in% exclude)

  callees <- callees         %>%
        select(package, fun) %>%
        distinct             %>%
        arrange(package, fun)

  return(callees)
}
