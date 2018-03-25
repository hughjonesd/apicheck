
context("Remote tests")


install_on <- function (mran, package, version) {
  if (mran) skip("MRAN not working for the moment...")
  force(package)
  force(version)
  run_in_fresh_cache(mran,
    expect_error(call_with_namespace(package, version, function (x) NULL, quiet = TRUE), NA,
          info = sprintf("mran: %s, package: %s, version: %s", mran, package, version))
  )
}


install_early_late <- function (package) {
  v <- available_versions(package)$version
  current_v <- tail(v, 1)
  install_on(mran = FALSE, package, current_v)
  early_v <- v[length(v) - 1]
  install_on(mran = FALSE, package, early_v)
  install_on(mran = FALSE, package, early_v)
}


test_that("Can install early/current versions using `remotes`, and early versions using `versions`", {
  skip_on_cran()

  install_early_late("clipr") # no compilation
  install_early_late("alineR") # no dependencies & small but needs compilation
})


test_that("Can install versions when package already installed and loaded", {
  skip_on_cran()

  clear_lib_dir()
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

  run_in_fresh_cache(mran = FALSE, when_fun_exists("clipr::dr_clipr", search = "parallel"))
})


test_that("Can set lib_dir", {
  skip_on_cran()
  skip_on_travis() # slow

  tempdir <- tempfile(pattern = "testing", tmpdir = normalizePath(tempdir()))
  dir.create(tempdir)
  old_ld <- set_lib_dir(tempdir)
  on.exit(set_lib_dir(old_ld))
  prepare <- try(cached_install("clipr", "0.4.0"))
  if (class(prepare) == "try-error") skip("Couldn't download package for testing")
  expect_true(dir.exists(file.path(tempdir, "clipr-0.4.0", "clipr")))
})
