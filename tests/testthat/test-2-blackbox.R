context("Blackbox tests")

old_lib_dir <- NULL
old_opts <- NULL

setup({
  old_opts <<- options(apicheck.use_cran = TRUE, repos = "https://cloud.r-project.org", cl.cores = 2)
  old_lib_dir <<- set_lib_dir(if (Sys.info()["sysname"] != "Windows") "testing_lib_dir" else NULL)
})


teardown({
  options(old_opts)
  if (Sys.info()["sysname"] == "Windows") clear_package_cache()
  set_lib_dir(old_lib_dir)
})


test_that("available_versions", {
  expect_silent(vns <- available_versions("longurl"))
  # check caching:
  expect_silent(vns2 <- available_versions("longurl"))
  expect_identical(vns, vns2)
  expect_identical(names(vns), c("version", "date"))
  expect_identical(vns, vns[order(vns$date), ])
})


test_that("get_version_at_date", {
  expect_silent(d <- get_version_at_date("clipr", "2017-01-01"))
  expect_identical(d, "0.3.1")
  expect_silent(d <- get_version_at_date("clipr", "2016-01-01"))
  expect_identical(d, "0.2.0")
})


test_that("arguments passed to install.packages", {
  expect_error(cached_install("fortunes", "1.5-1", repos = "BROKEN"))
  expect_error(fn_exists_at("fortunes::fortune", version = "1.5-0", repos = "BROKEN"))
  fn <- base::as.character
  expect_error(api_same_at("fortunes::fortune", version = "1.5-2", current_fn = fn, repos = "BROKEN"))

  skip("Skipping two tests where install.packages spews weird uncatchable errors")
  expect_warning(when_fn_exists("fortunes::fortune", repos = "BROKEN"))
  expect_warning(when_api_same("rbcb::get_currency", current_fn = fn, repos = "BROKEN"))
})


test_that("cached_install and call_with_namespace", {
  skip_on_cran()
  skip_on_travis() # slow
  # can't skip_if_mran_down() because it uses this very function, so instead:

  # expect possible warnings etc. but no errors
  # res is NULL if there is an error
  res <- expect_error(d1 <- cached_install("clipr", "0.4.0"), regexp = NA)
  if (is.null(res)) skip("cached_install failed, no point trying the others")
  expect_error(d2 <- cached_install("clipr", "0.4.0"), regexp = NA)
  expect_error(d3 <- cached_install("clipr", "0.2.0"), regexp = NA)
  expect_equal(d1, d2) # but not identical for some reason
  expect_false(isTRUE(all.equal(d1, d3)))
  unloadNamespace("clipr")

  test <- function (namespace) {
    stopifnot(is.environment(namespace))
    stop("Ran test ok")
  }
  # already downloaded
  expect_error(call_with_namespace("clipr", "0.4.0", test), regexp = "Ran test ok")
  expect_false(isNamespaceLoaded("clipr"))
  # not downloaded
  expect_error(call_with_namespace("clipr", "0.3.2", test), regexp = "Ran test ok")
  expect_false(isNamespaceLoaded("clipr"))
})


test_that("cached_install with attached namespace", {
  skip_on_cran()

  if (! require(withr)) skip("Couldn't attach withr")
  run_in_fresh_cache(cran = TRUE,
          expect_warning(cached_install("withr", "2.1.1"), "already loaded")
        )
})


test_that("Can call functions with different calling conventions", {
  skip_on_cran()

  # expect_identical doesn't work for functions
  expect_equal(
          get_fn_at("clipr::write_clip", version = "0.4.0"),
          get_fn_at("write_clip", "clipr", version = "0.4.0")
        )
})


test_that("fn_exists_at", {
  skip_on_cran()

  expect_true(fn_exists_at("clipr::dr_clipr", version = "0.4.0"))
  expect_false(fn_exists_at("clipr::dr_clipr", version = "0.2.0"))
})


test_that("api_same_at", {
  skip_on_cran()

  wc4 <- get_fn_at("clipr::write_clip", version = "0.4.0")
  wc011 <- get_fn_at("clipr::write_clip", version = "0.1.1")
  expect_false(api_same_at("clipr::write_clip", version = "0.1.1", current_fn = wc4)) # gained an argument
  dr_c <- get_fn_at("clipr::dr_clipr", version = "0.4.0")
  # should warn because dr_clipr didn't exist back then:
  expect_warning(x <- api_same_at("clipr::dr_clipr",  version = "0.1.1", current_fn = dr_c))
  expect_false(x)
})


test_that("when_api_same", {
  skip_on_cran()

  dr_c <- get_fn_at("clipr::dr_clipr", version = "0.4.0")
  wc   <- get_fn_at("clipr::write_clip", version = "0.4.0")

  expect_identical(suppressWarnings(when_api_same("clipr::dr_clipr", current_fn = dr_c,
        report = "brief")), "0.4.0") # new function, so we suppress warnings

  strategies <- c("binary", "forward", "backward", "all")

  for (search in strategies) {
    expect_identical(when_api_same("clipr::write_clip", current_fn = wc, search = search, report = "brief"),
          "0.2.0") # API change
  }

  results_wanted <- list(
    binary   = c("Known different", "Known same", "Assumed same", "Known same",
                rep("Assumed same", 4)),
    forward  = c("Known different", "Known same", rep("Assumed same", 6)),
    backward = c("Known different", rep("Known same", 7)),
    all      = c("Known different", rep("Known same", 7)),
    parallel = c("Known different", rep("Known same", 7))
  )
  for (search in strategies) {
    info <- paste("Search strategy was:", search)
    expect_error(res <- when_api_same("clipr::write_clip", current_fn = wc, search = search, report = "full"), NA,
          info = info)
    expect_s3_class(res, "data.frame") # no info arg :-(
    expect_identical(names(res), c("version", "date", "result"), info = info)
    # see below re clipr 0.1.0
    expect_identical(res$result[-1], results_wanted[[search]], info = info)
  }
})


test_that("when_fn_exists", {
  skip_on_cran()

  expect_equal(when_fn_exists("clipr::dr_clipr", report = "brief"), "0.4.0")

  strategies <- c("binary", "forward", "backward", "all")
  # we only test versions 0.1.1 and onwards because version 0.1.0 varies with use_cran
  # being TRUE or FALSE
  results_wanted <- list(
    binary   = c(rep("Assumed absent", 3), "Known absent", "Assumed absent", rep("Known absent", 2), "Known present"),
    forward  = c(rep("Known absent", 7), "Known present"),
    backward = c(rep("Assumed absent", 6), "Known absent", "Known present"),
    all      = c(rep("Known absent", 7), "Known present"),
    parallel = c(rep("Known absent", 7), "Known present")
  )
  for (search in strategies) {
    info <- paste("Search strategy was:", search)
    expect_error(res <- when_fn_exists("clipr::dr_clipr", search = search, report = "full"), NA, info = info)
    expect_s3_class(res, "data.frame")
    expect_identical(names(res), c("version", "date", "result"), info = info)
    expect_identical(res$result[-1], results_wanted[[search]], info = info)
  }
})


test_that("get_help_at", {
  skip_on_cran()

  expect_error(helpfile <- get_help_at("clipr::write_clip", "0.4.0"), NA)
  expect_s3_class(helpfile, "help_files_with_topic")
})


test_that("Can set lib_dir", {
  skip_on_cran()
  skip_on_travis() # slow

  tempdir <- tempfile(pattern = "testing", tmpdir = normalizePath(tempdir()))
  dir.create(tempdir)
  set_lib_dir(tempdir)
  prepare <- try(cached_install("clipr", "0.4.0"))
  if (class(prepare) == "try-error") skip("Couldn't download package for testing")
  expect_true(dir.exists(file.path(tempdir, "clipr-0.4.0", "clipr")))
})
