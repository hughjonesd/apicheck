
# utilities, bits and bobs, not tied to other specific parts of the package


mran_selected <- function () isTRUE(getOption("apicheck.use_mran", FALSE))


get_rcheology_rows <- memoise::memoise(function (name, package) {
  rcheology::rcheology[rcheology::rcheology$name == name & rcheology::rcheology$package == package, ]
})


assert_package <- function (package) {
  if (! requireNamespace(package, quietly = TRUE)) {
    stop("Could not load the `", package, "` library.\n", "Try `install.packages(\"", package, "\")`.")
  }
}


assert_not_core <- function (package) {
  if (is_core_package(package)) stop("`", package, "` is a core package and cannot be downloaded from CRAN or MRAN.")
}


parse_fun <- function (fun, single = TRUE) {
  fun_list <- strsplit(fun, "::", fixed = TRUE)
  if (! all(map_int(fun_list, length) == 2)) stop("Function name must have exactly 1 '::'")
  if (single) return(fun_list[[1]])
  return(map_dfr(fun_list, ~tibble::tibble(package = .[1], fun = .[2])))
}


