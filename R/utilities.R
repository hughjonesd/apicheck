

# utilities, bits and bobs, not tied to other specific parts of the package

mran_selected <- function () isTRUE(getOption("apicheck.use_mran", FALSE))


assert_package <- function (package) {
  if (! requireNamespace(package, quietly = TRUE)) {
    stop("Could not load the `", package, "` library.\n", "Try `install.packages(", package, ")`.")
  }
}


parse_fun <- function (fun) {
  if (! grepl("::", fun)) stop("No `package` specified or found in function name")
  strsplit(fun, "::", fixed = TRUE)[[1]]
}
