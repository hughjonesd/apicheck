

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
    f1 <- formals(get(x, ns1))
    f2 <- formals(get(x, ns2))
    identical(f1, f2)
  })
  api_ch_str <- "API changed"
  report$change[both_there] <- ifelse(api_changed, api_ch_str, NA_character_)
  report <- report[! is.na(report$change), ]

  report[, paste0("api", suffs)] <- NA_character_
  needs_api <- report$change == api_ch_str
  report$api_x[needs_api] <- sapply(report$function_x[needs_api], get_api_desc, ns1)
  report$api_y[needs_api] <- sapply(report$function_y[needs_api], get_api_desc, ns2)

  names(report) <- gsub("x$", v1, names(report))
  names(report) <- gsub("y$", v2, names(report))
  class(report) <- c("versions_report", class(report))
  return(report)
}

# a nice description of somebody's formals
get_api_desc <- function(nm, ns) {
  desc <- deparse(args(get(nm, ns)))
  desc <- desc[ -length(desc) ]
  desc <- paste(desc, collapse = "")
  desc <- trimws(desc)
  desc <- sub("^function\\s+", "", desc)
  if (nchar(desc, "width") > 40) {
    desc <- strtrim(desc, 40)
    substr(desc, 37, 40) <- " ..."
  }
  desc
}
