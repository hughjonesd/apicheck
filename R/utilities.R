
# utilities, bits and bobs, not tied to other specific parts of the package

#' @importFrom glue glue
NULL

# returns a lapply-like function and a possibly NULL cluster object
make_lapply_fun <- function (parallel, max_ncores) {
  lf <- if (! parallel) {
    list(lapply, NULL)
  } else {
    assert_package("parallel")
    # only way to find out if a cluster is registered?
    x <- try(parallel::clusterApply(NULL, 1, identity), silent = TRUE)
    cl <- if (class(x) == "try-error") {
      ncores <- getOption("mc.cores")
      if (is.null(ncores)) ncores <- min(parallel::detectCores() - 1, max_ncores)
      if (is.na(ncores)) ncores <- 2
      parallel::makeCluster(ncores)
    } else {
      NULL
    }

    parallel::clusterEvalQ(cl, library(apicheck))
    use_mran <- mran_selected()
    repos <- getOption("repos", "https://cloud.r-project.org")
    parallel::clusterCall(cl, options, apicheck.use_mran = use_mran, repos = repos)
    parallel::clusterExport(cl, "LIB_DIR", envir = environment())
    list(function (x, fun) parallel::parLapplyLB(cl, x, fun), cl)
  }

  return(lf)
}

# only stops a cluster if it was created by ourselves
maybe_stop_cluster <- function (cl) if (! is.null(cl)) parallel::stopCluster(cl)


mran_selected <- function () isTRUE(getOption("apicheck.use_mran", FALSE))


get_rcheology_rows <- memoise::memoise(function (name, package) {
  rcheology::rcheology[rcheology::rcheology$name == name & rcheology::rcheology$package == package, ]
})


assert_package <- function (package) {
  if (! requireNamespace(package, quietly = TRUE)) {
    stop(glue("Could not load the '{package}' library. Try:\n  install.packages('{package}')."))
  }
}


assert_not_core <- function (package) {
  if (is_core_package(package)) stop(
        glue("'{package}' is a core package and cannot be downloaded from CRAN or MRAN."))
}


unload_noncore_namespace <- function (package) {
  if (! is_core_package(package)) {
    tryCatch(
      unloadNamespace(package),
      error = function (e) {
        warning(glue("Could not unload package {package}, you may want to do it manually."),
            "Original error:\n", e$message)
      }
    )
  }
}


parse_fun <- function (fun, single = TRUE) {
  if (single) stopifnot(length(fun) == 1L)
  fun_list <- strsplit(fun, "::", fixed = TRUE)
  if (! all(map_int(fun_list, length) == 2L)) stop("Function name must have exactly 1 '::'")
  if (single) return(fun_list[[1]])
  return(map_dfr(fun_list, ~ tibble::tibble(package = .[1], fun = .[2])))
}


really_quietly <- function (env) {
  function (expr) {
    msg <- utils::capture.output(out <- utils::capture.output(eval(substitute(expr, env))),
          type = "message")
    return(list(msg = msg, out = out))
  }
}


say <- function (...) cat(..., "\n")
