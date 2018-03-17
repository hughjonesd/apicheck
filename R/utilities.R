

# utilities, bits and bobs, not tied to other specific parts of the package

mran_selected <- function () isTRUE(getOption("apicheck.use_mran", FALSE))


get_rcheology_rows <- memoise::memoise(function (name, package) {
  rcheology::rcheology[rcheology::rcheology$name == name & rcheology::rcheology$package == package, ]
})


assert_package <- function (package) {
  if (! requireNamespace(package, quietly = TRUE)) {
    stop("Could not load the `", package, "` library.\n", "Try `install.packages(", package, ")`.")
  }
}


assert_not_core <- function (package) {
  if (is_core_package(package)) stop("`", package, "` is a core package and cannot be downloaded from CRAN or MRAN.")
}

parse_fun <- function (fun) {
  if (! grepl("::", fun)) stop("No `package` specified or found in function name")
  strsplit(fun, "::", fixed = TRUE)[[1]]
}
