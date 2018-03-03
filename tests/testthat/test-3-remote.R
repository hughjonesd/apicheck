
context("Remote tests")

library(rlang)

run_nicely <- function(cran, expr) {
  expr <- enquo(expr)
  old_opts <- options(pastapi.use_cran = cran, repos = "https://cloud.r-project.org")
  old_lib_dir <- set_lib_dir(NULL) # can't avoid possibly putting LIB_DIR into options...
  clear_package_cache()
  on.exit({
    options(old_opts)
    set_lib_dir(old_lib_dir)
  })
  eval_tidy(expr)
}


install_on <- function (cran, package, version) {
  if (cran == FALSE) skip("MRAN not working for the moment...")
  force(package)
  force(version)
  run_nicely(cran,
    expect_error(call_with_namespace(package, version, function (x) NULL, quiet = TRUE), NA,
          info = sprintf("cran: %s, package: %s, version: %s", cran, package, version))
  )
}


install_early_late <- function (package) {
  v <- available_versions(package)$version
  current_v <- tail(v, 1)
  install_on(cran = TRUE, package, current_v)
  early_v <- v[length(v) - 1]
  install_on(cran = TRUE, package, early_v)
  install_on(cran = FALSE, package, early_v)
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


test_that("Multiple parallel remote installs", {
  skip_on_cran()

  run_nicely(cran = TRUE, when_fn_exists("clipr::dr_clipr", search = "parallel"))
})
