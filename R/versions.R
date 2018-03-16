

#' Report available versions
#'
#' This returns packages ordered by date, using either \href{https://mran.microsoft.com}{MRAN} or
#' \href{http://crandb.r-pkg.org}{metacran}.
#' Results are cached so as to
#' relieve pressure on the server. If `options("apicheck.use_mran")` is `TRUE`,
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
    vns_df <- if (mran_selected()) av_mran(package) else av_metacran(package)
    if (nrow(vns_df) == 0L) stop(sprintf("Could not find any available versions for '%s'", package))
    vns_df$date <- as.Date(vns_df$date)
    vns_df <- vns_df[order(vns_df$date), ]
    if (mran_selected()) vns_df <- vns_df[vns_df$Date >= "2014-09-17", ]

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


previous_version <- function (package) {
  vns_df <- available_versions(package)
  return(vns_df$version[nrow(vns_df) - 1])
}


current_version <- function (package) {
  vns_df <- available_versions(package)
  return(vns_df$version[nrow(vns_df)])
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
