
context("Whitebox tests")

old_lib_dir <- NULL
old_opts <- NULL

setup({
  old_lib_dir <<- get_lib_dir()
  old_opts <<- options(cl.cores = 2) # for travis
  # we want a new directory for these tests, otherwise last test will delete your pre-cached files
  set_lib_dir(NULL)
})


teardown({
  clear_lib_dir()
  options(old_opts)
  set_lib_dir(old_lib_dir)
})


test_that("parse_fn", {
  expect_identical(apicheck:::parse_fn("foo::bar"), c("foo", "bar"))
})


test_that("Failure to install a file does not leave a directory on disk", {
  tempdir <- tempfile(pattern = "testing", tmpdir = normalizePath(tempdir()))
  dir.create(tempdir)
  set_lib_dir(tempdir)
  fail <- function (...) stop("Some crazy failure")
  with_mock(
    `versions::install.versions` = fail,
    `remotes::install_version`   = fail,
  {
    expect_error(get_fn_at("expand_urls", "longurl", "0.3.0"), "Some crazy failure")
    expect_false(dir.exists(file.path(tempdir, "longurl")))
  })
})


test_that("na_binary_search", {
  run_bs <- function (x) {
    f <- function (n) x[n]
    apicheck:::na_binary_search(1L, length(!!x), f)
  }

  # TRUE can be "assumed FALSE" -1, if L of a FALSE; but never known FALSE -2;
  # FALSE can be "assumed TRUE" 1, if R of a TRUE; but never known TRUE 2;
  # NA can be -1, 0 or 1 but never 2 or -2
  check_result <- function (input) {
    output <- run_bs(input)
    expect_length(output, length(input))
    expect_type(output, "integer")
    expect_true(all(output %in% seq(-2L, 2L)))
    nas <- is.na(input)
    expect_true(all(output[nas] %in% seq(-1L, 1L)))
    output <- output[! nas]
    input <- input[! nas]
    if (length(output) == 0) return()
    expect_true(all(!! output[input] > -2L))
    expect_true(all(!! output[! input] < 2L))
    # check assumed TRUE/FALSE are to right/left of a known TRUE/FALSE value:
    assumed_true <- which( output == 1L)
    assumed_false <- which(output == -1L)
    # if the args to which are zero-length, then throw an error,
    # as there are no known TRUE/FALSE values:
    expect_true(all(assumed_true > min(which(input), length(input) + 1)))
    expect_true(all(assumed_false < max(which(! input), 0)))
  }

  check_result(TRUE)
  check_result(FALSE)
  check_result(NA)
  for (ln in 2:10) {
    for (i in 1:30) {
      x <- sample(c(TRUE, FALSE, NA), ln, replace = TRUE)
      check_result(x)
    }
  }
})


test_that("search_versions", {

    test_vars <- function (search, ...) {
      x <- as.logical(list(...))
      test <- function (y) x[y]
      apicheck:::search_versions(1:3, test, search)
    }

    expect_equal(test_vars("forward", TRUE, FALSE, TRUE), c(2L, 1L, 1L))
    expect_equal(test_vars("forward", TRUE, NA, TRUE), c(2L, 1L, 1L))
    expect_equal(test_vars("forward", FALSE, NA, TRUE), c(-2L, 0L, 2L))

    expect_equal(test_vars("backward", TRUE, FALSE, TRUE), c(-1L, -2L, 2L))
    expect_equal(test_vars("backward", TRUE, FALSE, TRUE), c(-1L, -2L, 2L))
    expect_equal(test_vars("backward", TRUE, NA, FALSE), c(-1L, -1L, -2L))
})


test_that("search_all", {
  test_vars <- function (search, ...) {
    x <- as.logical(list(...))
    test <- function (y) x[y]
    apicheck:::search_all(seq_along(x), test, search)
  }

  for (strat in c("all", "parallel")) expect_equal(test_vars(strat, FALSE, NA, TRUE), c(-2L, 0, 2L))
})


test_that("clear_lib_dir", {
  ld <- get_lib_dir()
  cat("blah", file = file.path(ld, "notarealpackage-0.2.0"))
  clear_lib_dir()
  expect_length(list.files(ld), 0)
})
