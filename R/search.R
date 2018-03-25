
# functions that search through multiple versions

#' @importFrom zeallot %<-%
NULL

#' @param search "binary", "forward", "backward", "all" or "parallel". See Search strategies.
#' @param report "brief" or "full". See Value.
#' @param progress Print a progress bar.
#' @param min_version Lowest version to check.
#' @param max_version Highest version to check.
#'
#' @return
#' If `report` is "brief", the earliest "known good" version.
#' Otherwise, a data frame of versions with a full results column.
#'
#' @name search_doc
NULL


#' @section Parallelism:
#' For parallel search, you can set up your own parallel
#' cluster by using [parallel::setDefaultCluster()]; otherwise one will be created, using
#' `getOption("cl.cores")` cores if that is set. If you
#' set up your own cluster, it will not be stopped automatically (see
#' [parallel::stopCluster()]).
#'
#' @name parallel_doc
NULL

#' Compare APIs across package versions
#'
#' `when_api_same` reports package versions where the API of a function was the same as now (or
#' the same as `current_fun`).
#'
#' @inherit params_doc params
#' @inherit search_doc params details return
#'
#' @inherit same_api_doc details
#'
#' @section Speed:
#' This function may download and install multiple versions from MRAN, so it is likely to be slow
#' when first used (and even afterwards if library loading is slow). Using `search = "parallel"`
#' may help, but not if the network is the bottleneck: see
#' \href{here}{https://hughjonesd.github.io/apicheck/performance2.html} for details.
#'
#' @section Search strategies:
#' \itemize{
#'   \item `"forward"` (`"backward"`) searches incrementally from the earliest (latest) version.
#'   \item `"binary"` does a binary search from the midpoint.
#' }
#'
#' These
#' strategies assume that API changes happen just once - i.e. once a function exists or API is the same as now,
#' it will stay so in future versions. This allows them to stop before searching every version.
#'
#' \itemize{
#'   \item `"all"` searches every version.
#'   \item `"parallel"` searches every version in parallel using [parallel::parLapply()].
#' }
#'
#' @inheritSection parallel_doc Parallelism
#'
#' @export
#'
#' @examples
#' \dontrun{
#' when_api_same("read.dta", "foreign")
#' }
when_api_same <- function (
        fun,
        package,
        current_fun  = NULL,
        search      = c("binary", "forward", "backward", "all", "parallel"),
        report      = c("full", "brief"),
        quiet       = TRUE,
        progress    = interactive() && search != "parallel",
        min_version = NULL,
        max_version = NULL,
        ...
      ) {
  search <- match.arg(search)
  report   <- match.arg(report)
  if (missing(package)) c(package, fun) %<-% parse_fun(fun)
  force(current_fun)

  test <- wrap_test(
    function (version) suppressWarnings(api_same_at(fun, package = package, version = version,
          current_fun = current_fun, ...))
  )
  res <- run_search(package, test, search, progress, min_version, max_version)

  res <- clean_up_result(res, package, report,
        labels = c("Known different", "Assumed different", "Unknown", "Assumed same", "Known same"),
        min_version, max_version)

  return(res)
}


#' `when_fun_exists` reports package versions where a function exists.
#'
#' @rdname when_api_same
#' @export
#'
#' @examples
#' \dontrun{
#' when_fun_exists('read.dta', 'foreign')
#' }
when_fun_exists <- function (
          fun,
          package,
          search      = c("binary", "forward", "backward", "all", "parallel"),
          report      = c("full", "brief"),
          quiet       = TRUE,
          progress    = interactive() && search != "parallel",
          min_version = NULL,
          max_version = NULL,
          ...
        ) {
  search <- match.arg(search)
  report   <- match.arg(report)
  if (missing(package)) c(package, fun) %<-% parse_fun(fun)

  test <- wrap_test(function (version) fun_exists_at(fun, package = package, version = version, ...))
  res <- run_search(package, test, search, progress, min_version, max_version)
  res <- clean_up_result(res, package, report,
        labels = c("Known absent", "Assumed absent", "Unknown", "Assumed present", "Known present"),
        min_version, max_version)

  return(res)
}


wrap_test <- function (test_fun) {
  function (version) {
    res <- try(test_fun(version))
    if (class(res) == 'try-error') {
      return(NA)
    }
    return(res)
  }
}


suitable_versions <- function(package, min_version, max_version) {
  vns_df <- available_versions(package)
  versions <- as.package_version(vns_df$version)
  if (! is.null(min_version)) vns_df <- vns_df[versions >= min_version, ]
  if (! is.null(max_version)) vns_df <- vns_df[versions <= max_version, ]

  return(vns_df)
}


run_search <- function (package, test, search, progress, min_version, max_version) {
  versions <- suitable_versions(package, min_version, max_version)$version
  pb <- NULL
  if (progress && ! search == "parallel") {
    pb <- utils::txtProgressBar(style = 3)
    inc <- 1/length(versions)
    test_inner <- test
    test <- function (x) {
      res <- test_inner(x)
      g <- utils::getTxtProgressBar(pb)
      utils::setTxtProgressBar(pb, g + inc)
      return(res)
    }
  }

  res <- switch(search,
          binary   = binary_search_versions(versions, test),
          backward = ,
          forward  = search_versions(versions, test, search),
          parallel = ,
          all = search_all(versions, test, search)
  )
  if (! is.null(pb)) {
    utils::setTxtProgressBar(pb, 1)
    close(pb)
  }

  return(res)
}



# the next functions run a test and return results for all versions:
# -2 = FALSE, -1 assumed FALSE, 0 = NA, 1 = assumed TRUE, 2 = TRUE
binary_search_versions <- function(versions, test) {
  test_wrap <- function (x) test(versions[x])
  result <- na_binary_search(1L, length(versions), test_wrap)

  return(result)
}


na_binary_search <- function (l, r, test) {
  if (l > r) return(integer(0)) # this can happen e.g. after l = 2, r = 3
  m <- as.integer(floor((l + r)/2))
  res <- test(m)
  if (isTRUE(res)) return(c(na_binary_search(l, m - 1, test), 2L, rep(1L, r - m)))
  if (isTRUE(! res)) return(c(rep(-1L, m - l), -2L, na_binary_search(m + 1, r, test)))
  if (is.na(res)) {
    return(c(na_binary_search(l, m - 1, test), 0L, na_binary_search(m + 1, r, test)))
  }
}


search_versions <- function (versions, test, search) {
  res <- logical(0)
  stop_test <- switch(search,
          forward  = isTRUE,
          backward = function (x) isTRUE(! x)
        )
  assume_rest <- switch(search, forward = 1L, backward = -1L, NA)
  if (search == "backward") versions <- rev(versions)

  for (i in seq_along(versions)) {
    res[i] <- test(versions[i])
    if (stop_test(res[i]) || i == length(versions)) {
      res <- ifelse(is.na(res), 0L, 4 * as.integer(res) - 2)
      if (i < length(versions)) res[seq(i + 1L, length(versions))] <- assume_rest
      break;
    }
  }
  if (search == "backward") res <- rev(res)

  return(res)
}


utils::globalVariables(c("lapply_fun", "cl"))

search_all <- function (versions, test, search) {
  parallel <- search == "parallel"
  c(lapply_fun, cl) %<-% make_lapply_fun(parallel = parallel, max_ncores = length(versions))

  res <- lapply_fun(versions, test)
  res <- as.logical(res)
  res <- ifelse(is.na(res), 0L, 4L * as.integer(res) - 2L)
  if (parallel) maybe_stop_cluster(cl)

  return(res)
}

clean_up_result <- function (res, package, report, labels, min_version, max_version) {
  vns_df <- suitable_versions(package, min_version, max_version)
  if (report == "brief") {
    first_known_good(package, res, vns_df)
  } else {
    versions_with_result(package, res, labels, vns_df)
  }
}


first_known_good <- function (package, res, vns_df) {
  return(vns_df$version[match(2L, res)])
}


versions_with_result <- function (package, res, labels, vns_df) {
  res <- factor(res, levels = seq(-2L, 2L), labels = labels)
  vns_df$result <- as.character(res)

  return(vns_df)
}
