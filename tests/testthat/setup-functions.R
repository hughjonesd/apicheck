

skip_if_mran_down <- function () {
  prepare <- try(load_version_namespace("clipr", "0.4.0", cache = FALSE), silent = TRUE)
  if (class(prepare) == "try-error") skip("Couldn't download test package, MRAN may be down")
}
