context("Blackbox tests")

old_lib_dir <- NULL
old_opts <- NULL

setup({
  old_opts <<- options(apicheck.use_mran = FALSE, repos = "https://cloud.r-project.org",
        mc.cores = 2)
  old_lib_dir <<- set_lib_dir(if (Sys.info()["sysname"] != "Windows") "testing_lib_dir" else NULL)
})


teardown({
  options(old_opts)
  if (Sys.info()["sysname"] == "Windows") clear_lib_dir()
  set_lib_dir(old_lib_dir)
})


test_that("available_versions", {
  for (mran in c(TRUE, FALSE)) {
    withr::with_options(list(apicheck.use_mran = mran), {
      info <- paste("use_mran:", mran)
      expect_error(vns <- available_versions("longurl"), regexp = NA, info = info)
      expect_identical(names(vns), c("version", "date"), info = info)
      expect_identical(vns, vns[order(vns$date), ], info = info)
      expect_identical(sapply(vns, class), c(version = "character", date = "Date"))
      # check caching:
      expect_error(vns2 <- available_versions("longurl"), regexp = NA, info = info)
      expect_identical(vns, vns2, info = info)

    })
  }

  # core versions:
  expect_silent(vns_core <- available_versions("base"))
  expect_identical(names(vns_core), c("version", "date"))
  expect_identical(vns_core, vns_core[order(vns_core$date), ])
  expect_identical(sapply(vns_core, class), c(version = "character", date = "Date"))
})


test_that("version_at_date", {
  expect_silent(d <- version_at_date("clipr", "2017-01-01"))
  expect_identical(d, "0.3.1")
  expect_silent(d <- version_at_date("clipr", "2016-01-01"))
  expect_identical(d, "0.2.0")
  # core packages
  expect_silent(d <- version_at_date("utils", "2017-01-01"))
  expect_identical(d, "3.3.2")
})


test_that("arguments passed to install.packages", {
  expect_error(cached_install("fortunes", "1.5-1", repos = "BROKEN"))
  expect_error(fun_exists_at("fortunes::fortune", version = "1.5-0", repos = "BROKEN"))
  fun <- base::as.character
  expect_error(api_same_at("fortunes::fortune", version = "1.5-2", current_fun = fun,
        repos = "BROKEN"))

  skip("Skipping two tests where install.packages spews weird uncatchable errors")
  expect_warning(when_fun_exists("fortunes::fortune", repos = "BROKEN"))
  expect_warning(when_api_same("rbcb::get_currency", current_fun = fun, repos = "BROKEN"))
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
  skip("Don't yet know how to load namespace when package is already loaded... ")

  if (! require(withr)) skip("Couldn't attach withr")
  run_in_fresh_cache(mran = FALSE,
          expect_warning(cached_install("withr", "2.1.1"), "already loaded")
        )
})


test_that("Can call functions with different calling conventions", {
  skip_on_cran()

  # expect_identical doesn't work for functions
  expect_equal(
          fun_at("clipr::write_clip", version = "0.4.0"),
          fun_at("write_clip", "clipr", version = "0.4.0")
        )
})


test_that("fun_exists_at", {
  skip_on_cran()

  expect_true(fun_exists_at("clipr::dr_clipr", version = "0.4.0"))
  expect_false(fun_exists_at("clipr::dr_clipr", version = "0.2.0"))
  # core packages
  expect_true(fun_exists_at("base::strrep", "3.3.0"))
  expect_false(fun_exists_at("base::strrep", "3.2.5"))
})


test_that("api_same_at", {
  # core packages
  dbo <- fun_at("base::debugonce", "3.4.3", allow_core = TRUE)
  expect_true(api_same_at("base::debugonce", "3.4.0", current_fun = dbo))
  expect_false(api_same_at("base::debugonce", "3.3.3", current_fun = dbo))
  expect_warning(x <- api_same_at("base::strrep", "3.2.5", current_fun =
        fun_at("base::strrep", "3.4.3", allow_core = TRUE)))
  expect_false(x)

  skip_on_cran()

  wc4 <- fun_at("clipr::write_clip", version = "0.4.0")
  wc011 <- fun_at("clipr::write_clip", version = "0.1.1")
  # gained an argument:
  expect_false(api_same_at("clipr::write_clip", version = "0.1.1", current_fun = wc4))
  dr_c <- fun_at("clipr::dr_clipr", version = "0.4.0")
  # should warn because dr_clipr didn't exist back then:
  expect_warning(x <- api_same_at("clipr::dr_clipr",  version = "0.1.1", current_fun = dr_c))
  expect_false(x)
})


test_that("when_api_same", {
  skip_on_cran()

  dr_c <- fun_at("clipr::dr_clipr", version = "0.4.0")
  wc   <- fun_at("clipr::write_clip", version = "0.4.0")

  expect_identical(suppressWarnings(when_api_same("clipr::dr_clipr", current_fun = dr_c,
        report = "brief")), "0.4.0") # new function, so we suppress warnings

  strategies <- c("binary", "forward", "backward", "all")
  dbo <- fun_at("base::debugonce", "3.4.3", allow_core = TRUE)

  for (search in strategies) {
    expect_identical(when_api_same("clipr::write_clip", current_fun = wc, search = search,
          report = "brief"), "0.2.0") # API change
    # max_version to avoid being struck by new Rs in rcheology!
    expect_identical(when_api_same("base::debugonce", search = search, report = "brief",
          max_version = "3.4.3", current_fun = dbo), "3.4.0")
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
    expect_error(res <- when_api_same("clipr::write_clip", current_fun = wc, search = search,
          report = "full"), NA, info = info)
    expect_s3_class(res, "data.frame") # no info arg :-(
    expect_identical(names(res), c("version", "date", "result"), info = info)
    # see below re clipr 0.1.0
    expect_identical(res$result[-1], results_wanted[[search]], info = info)
  }
})


test_that("when_fun_exists", {
  skip_on_cran()

  strategies <- c("binary", "forward", "backward", "all")

  for (search in strategies) {
    info <- paste("Search strategy was:", search)
    expect_identical(when_fun_exists("clipr::dr_clipr", search = search, report = "brief"), "0.4.0")
    # max_version to avoid being struck by new R versions in rcheology:
    expect_identical(when_fun_exists("base::strrep", search = search, report = "brief",
          max_version = "3.4.3"), "3.3.0", info = info)
  }

  # we only test versions 0.1.1 and onwards because version 0.1.0 varies with use_mran
  # being TRUE or FALSE
  results_wanted <- list(
    binary   = c(rep("Assumed absent", 3), "Known absent", "Assumed absent", rep("Known absent", 2),
          "Known present"),
    forward  = c(rep("Known absent", 7), "Known present"),
    backward = c(rep("Assumed absent", 6), "Known absent", "Known present"),
    all      = c(rep("Known absent", 7), "Known present"),
    parallel = c(rep("Known absent", 7), "Known present")
  )
  for (search in strategies) {
    info <- paste("Search strategy was:", search)
    expect_error(res <- when_fun_exists("clipr::dr_clipr", search = search, report = "full"), NA,
          info = info)
    expect_s3_class(res, "data.frame")
    expect_identical(names(res), c("version", "date", "result"), info = info)
    expect_identical(res$result[-1], results_wanted[[search]], info = info)
  }
})


test_that("parallel search with own cluster", {
  cl <- parallel::makeCluster(2L)
  parallel::setDefaultCluster(cl)
  expect_silent(res <- when_fun_exists("clipr::dr_clipr", search = "parallel"))
  parallel::stopCluster(cl)
})


test_that("min_version and max_version work", {
  expect_error(res <- when_fun_exists("clipr::dr_clipr", search = "binary", min_version = "0.3.2",
        report = "full"), NA)
  expect_true(all(res$version >= as.package_version("0.3.2")))
  expect_error(res <- when_fun_exists("clipr::dr_clipr", search = "binary", max_version = "0.2.0",
        report = "full"), NA)
  expect_true(all(res$version <= as.package_version("0.2.0")))

  expect_error(res <- when_fun_exists("base::debugonce", search = "binary", min_version = "3.2.5",
        report = "full"), NA)
  expect_true(all(res$version >= as.package_version("3.2.5")))
  expect_error(res <- when_fun_exists("base::debugonce", search = "binary", max_version = "2.0.0",
        report = "full"), NA)
  expect_true(all(res$version <= as.package_version("2.0.0")))
})


test_that("help_at", {
  expect_error(help_at("utils::alarm"), "is a core package")
  skip_on_cran()

  expect_error(helpfile <- help_at("clipr::write_clip", "0.4.0"), NA)
  expect_s3_class(helpfile, "help_files_with_topic")
})


test_that("compare_versions", {
  expect_error(vr <- compare_versions("clipr", "0.2.1", "0.3.0"), NA)
  expect_s3_class(vr, "data.frame")
  expect_identical(ncol(vr), 5L)
  expect_identical(nrow(vr), 1L)
  expect_identical(vr[[1, 2]], "clipr_available")
  expect_identical(vr[[1, 3]], "Added")
})

test_that("package_report", {
  skip_on_cran()

  # produces invisible warnings, but seems to work
  expect_error(pr <- package_report("clipr-source", progress = FALSE), NA)
  expect_s3_class(pr, "data.frame")
  expect_identical(names(pr), c("package", "version", "funs"))
  # leaves only utils
  expect_error(pr2 <- package_report("clipr-source", exclude = c("base", "rstudioapi"),
        progress = FALSE), NA)
  expect_error(pr <- package_report("clipr-source", parallel = TRUE, exclude = "base"), NA)
})
