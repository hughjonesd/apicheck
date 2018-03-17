
# For high level interface with formatted output


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

  report1 <- data.frame(f = objs1, f1 = objs1, stringsAsFactors = FALSE)
  report2 <- data.frame(f = objs2, f2 = objs2, stringsAsFactors = FALSE)
  report <- merge(report1, report2, all = TRUE, suffixes = suffs, by = "f")
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


