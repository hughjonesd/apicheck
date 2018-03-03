
context("Remote tests")

install_on <- function (CRAN, package, version) {
  old_opts <- options(pastapi.use_CRAN = CRAN)
  old_lib_dir <- set_lib_dir(NULL) # can't avoid possibly putting LIB_DIR into options...
  clear_package_cache()
  on.exit({
    options(old_opts)
    set_lib_dir(old_lib_dir)
  })
  expect_error(call_with_namespace(!!package, !!version, function (x) NULL, quiet = TRUE), NA,
        info = paste("use_CRAN was", CRAN))
}


install_early_late <- function (package) {
  v <- available_versions(package)$version
  current_v <- tail(v, 1)
  install_on(CRAN = TRUE, package, current_v)
  early_v <- v[length(v) - 1]
  install_on(CRAN = TRUE, package, early_v)
  install_on(CRAN = FALSE, package, early_v)
}


test_that("Can install early and current versions using devtools, and early versions using `versions`", {
  skip_on_cran()

  install_early_late("clipr") # no compilation
  install_early_late("alineR") # no dependencies & small but needs compilation
})

test_that("Can install versions when package already installed and loaded", {
  skip_on_cran()

  clear_package_cache()
  if (! "clipr" %in% rownames(installed.packages())) {
    tempdir <- tempfile("testing", tmpdir = normalizePath(tempdir()))
    dir.create(tempdir)
    install.packages("clipr", repos = "https://cloud.r-project.org", lib = tempdir)
    on.exit(remove.packages("clipr", lib = tempdir))
    library(clipr, lib.loc = tempdir)
  } else {
    library(clipr)
  }

  install_early_late("clipr")
})

