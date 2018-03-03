


#' @param search "binary", "forward", "backward", "all" or "parallel". See Details.
#' @param report "brief" or "full". See Value.
#' @details
#' "forward" ("backward") searches incrementally
#' from the earliest (latest) version; "binary" does a binary search from the midpoint. These search
#' strategies assume that API changes happen just once - i.e. once a function exists or API is the same as now,
#' it will stay so in future versions. "all" searches every version and makes no assumptions. "parallel" searches
#' every version in parallel using \code{\link[parallel]{parLapply}}.
#'
#' @return
#' If \code{report} is "brief", the earliest "known good" version.
#' Otherwise, a data frame of versions with a full results column.
#'
#' @name search_doc
NULL


#' Compare function existence and APIs across package versions
#'
#' \code{when_api_same} reports package versions where the API of a function was the same as now (or
#' the same as \code{current_fn}).
#'
#' @inherit basic_params_doc params
#' @inherit version_params_doc params
#' @inherit current_fn_doc params
#'
#' @inherit same_api_doc details
#' @inherit slow_warning_doc details
#' @inherit search_doc params details return
#'
#' @export
#'
#' @examples
#' \dontrun{
#' when_api_same("read.dta", "foreign")
#' }
when_api_same <- function (
        fn,
        package,
        current_fn = NULL,
        search     = c("binary", "forward", "backward", "all", "parallel"),
        report     = c("full", "brief"))
      {
  search <- match.arg(search)
  report   <- match.arg(report)
  if (missing(package)) c(package, fn) %<-% parse_fn(fn)
  force(current_fn)

  test <- wrap_test(
    function (version) suppressWarnings(api_same_at(fn, package = package, version = version,
          current_fn = current_fn))
  )
  res <- run_search(package, test, search)

  res <- clean_up_result(res, package, report, labels = c("Known different", "Assumed different", "Unknown",
    "Assumed same", "Known same"))

  return(res)
}


#' \code{when_fn_exists} reports the first package version where a function exists.
#'
#' @inherit basic_params_doc params
#'
#' @inherit slow_warning_doc details
#' @inherit search_doc params details return
#'
#' @rdname when_api_same
#' @export
#'
#' @examples
#' \dontrun{
#' when_fn_exists('read.dta', 'foreign')
#' }
when_fn_exists <- function (
        fn,
        package,
        search = c("binary", "forward", "backward", "all", "parallel"),
        report = c("full", "brief"))
      {
  search <- match.arg(search)
  report   <- match.arg(report)
  if (missing(package)) c(package, fn) %<-% parse_fn(fn)

  test <- wrap_test(function (version) fn_exists_at(fn, package = package, version = version))
  res <- run_search(package, test, search)

  res <- clean_up_result(res, package, report,
        labels = c("Known absent", "Assumed absent", "Unknown", "Assumed present", "Known present"))

  return(res)
}


wrap_test <- function (test_fn) {
  function (version) {
    res <- try(test_fn(version))
    if (class(res) == 'try-error') {
      return(NA)
    }
    return(res)
  }
}


run_search <- function (package, test, search) {
  versions <- mran_versions(package)$version
  res <- switch(search,
          binary   = binary_search_versions(versions, test),
          backward = ,
          forward  = search_versions(versions, test, search),
          parallel = ,
          all = search_all(versions, test, search)
  )

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
  if (is.na(res)) return(c(na_binary_search(l, m - 1, test), 0L, na_binary_search(m + 1, r, test)))
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


search_all <- function (versions, test, search) {
  lapply_fn <- if (search == "parallel") {
    if (! requireNamespace('parallel', quietly = TRUE)) stop("Could not load `parallel` namespace")
    cl <- parallel::makeCluster(getOption("cl.cores", if (is.na(nc <- parallel::detectCores())) 2 else nc - 1))
    parallel::clusterEvalQ(cl, library(pastapi))
    function (x, fun) parallel::parLapply(cl, x, fun)
  } else {
    lapply
  }

  res <- lapply_fn(versions, test)
  res <- as.logical(res)
  res <- ifelse(is.na(res), 0L, 4 * as.integer(res) - 2)

  return(res)
}

clean_up_result <- function (res, package, report, labels) {
  if (report == "brief") {
    first_known_good(package, res)
  } else {
    versions_with_result(package, res, labels = labels)
  }
}


first_known_good <- function (package, res) {
  vns <- mran_versions(package)
  return(vns$version[match(2L, res)])
}


versions_with_result <- function (package, res, labels) {
  vns <- mran_versions(package)
  res <- factor(res, levels = seq(-2L, 2L), labels = labels)
  vns$result <- as.character(res)

  return(vns)
}
