


run_in_fresh_cache <- function(cran, expr) {
  expr <- rlang::enquo(expr)
  old_opts <- options(pastapi.use_cran = cran, repos = "https://cloud.r-project.org", cl.cores = 2)
  old_lib_dir <- set_lib_dir(NULL) # can't avoid possibly putting LIB_DIR into options...
  clear_package_cache()
  on.exit({
    options(old_opts)
    set_lib_dir(old_lib_dir)
  })
  rlang::eval_tidy(expr)
}

skip_if_mran_down <- memoise::memoise( function () {
  # small 17K package
  tempdir <- tempfile(pattern = "testing", tmpdir = normalizePath(tempdir()))
  dir.create(tempdir)
  prepare <- try(versions::install.versions("enrichwith", "0.0.3", lib = tempdir), silent = TRUE)
  if (class(prepare) == "try-error") skip("Couldn't download test package, MRAN may be down")
})
