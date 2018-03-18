
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
  version = previous_version(package),
  version2 = NULL,
  quiet    = TRUE,
  ...
) {
  # if NULL, version2 is latest and version is previous
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
  suffs <- c("_x", "_y")
  get_funs <- function (ns) Filter(function (x) is.function(get(x, ns)), getNamespaceExports(ns))
  objs1 <- get_funs(ns1)
  objs2 <- get_funs(ns2)

  report1 <- tibble::tibble(f = objs1, f1 = objs1)
  report2 <- tibble::tibble(f = objs2, f2 = objs2)
  report <- full_join(report1, report2, all = TRUE, suffix = suffs, by = "f")
  report <- report[, 2:3]
  names(report) <- paste0("function", suffs)
  report$change <- NA_character_
  report$change[is.na(report$function_x)] <- "Added"
  report$change[is.na(report$function_y)] <- "Removed"
  both_there <- is.na(report$change)

  api_changed <- sapply(report[both_there, 1, drop = TRUE], function (x){
    f1 <- get_fun_in_ns(x, ns1)
    f2 <- get_fun_in_ns(x, ns2)
    is_api_same(f1, f2)
  })
  api_ch_str <- "API changed"
  report$change[both_there] <- ifelse(api_changed, api_ch_str, NA_character_)
  report <- report[! is.na(report$change), ]

  report[, paste0("api", suffs)] <- NA_character_
  needs_api <- report$change == api_ch_str
  report$api_x[needs_api] <- sapply(report$function_x[needs_api], function (x) get_api_desc(get(x, ns1)))
  report$api_y[needs_api] <- sapply(report$function_y[needs_api], function (x) get_api_desc(get(x, ns2)))

  names(report) <- gsub("x$", v1, names(report))
  names(report) <- gsub("y$", v2, names(report))
  class(report) <- c("versions_report", class(report))
  return(report)
}


# a nice description of a function's formals
get_api_desc <- function(fun) {
  desc <- deparse(args(fun))
  desc <- desc[ -length(desc) ]
  desc <- paste(desc, collapse = "")
  desc <- trimws(desc)
  desc <- sub("^function\\s+", "", desc)
  if (nchar(desc, "width") > 40) {
    desc <- strtrim(desc, 40)
    substr(desc, 37, 40) <- " ..."
  }

  return(desc)
}


#' Report on backwards compatibility of a source package
#'
#' @param path Path to the root of an R package.
#' @param include Packages to include. By default, all are included except core packages and calls within the package itself.
#' @param exclude Packages to exclude from checking. Overrides `include`.
#' @param cutoff_date Don't check versions before this date (default: 2 years ago). Set to `NULL` for no cutoff.
#' @param max_versions Check at most this number of previous versions. Set to `NULL` for no limit.
#' @param ... Arguments passed to [call_with_namespace()].
#'
#' @details
#' This function lists the external function calls from a source package using [pkgapi::map_package()].
#' It then checks back-compatibility of each call us
#' Only function calls which are qualified with `::` will be detected, so e.g. `dplyr::filter`
#' will be
#'
#' @return A data frame with three rows:
#' * `package` for the external package
#' * `version` for the latest version which caused problems or `NA` if there were no problems
#' * `funs`    a list-column of function names where the API changed
#' @export
#'
#' @examples
package_report <- function (
        path = ".",
        include,
        exclude = core_packages(),
        cutoff_date  = Sys.Date() - 2 * 365,
        max_versions = NULL,
        ...
      ) {
  callees <- package_callees(path, include, exclude)
  packages <- unique(callees$package)

  results <- lapply(packages, function (package) {
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
    if (! is.null(max_versions)) versions <- slice(versions, seq_len(max_versions))
    are_apis_same <- function (ns) {
      funs <- lapply(fun_names, possibly(get_fun_in_ns, otherwise = NA), ns)
      nn <- ! is.na(funs)
      funs[nn] <- map2_lgl(funs[nn], current_funs[nn], is_api_same)
    }
    problem_funs <- character(0)
    problem_version <- NA_character_
    for (vn in seq_len(nrow(versions))) {
      version <- versions$version[vn]
      apis_same <- call_with_namespace(package, version, test = are_apis_same, ...)
      problem_funs <- fun_names[! apis_same]
      if (length(problem_funs) > 0) {
        problem_version <- version
        break
      }
    }
    return(list(version = problem_version, funs = problem_funs))
  })
  names(results) <- packages

  report <- tibble::tibble(
    package = packages,
    version = map_chr(results, "version"),
    funs    = map(results, "funs")
  )

  return(report)
}


package_callees <- function(path, include, exclude) {
  assert_package("pkgapi")

  fun_map <- pkgapi::map_package(path)
  callees <- fun_map$calls %>% select(to)
  callees[, c("package", "fun")] <- parse_fun(callees$to, single = FALSE)
  if (! missing(include)) callees <- filter(callees, package %in% include)
  itself <- desc::desc_get("Package", path)
  # "NA" is dynamically created; "" is local:
  # "NA" can also be an external function that is imported :-(
  exclude <- c("", itself, exclude)
  callees <- filter(callees, ! package %in% exclude)

  callees <- callees         %>%
        select(package, fun) %>%
        distinct             %>%
        arrange(package, fun)

  return(callees)
}
