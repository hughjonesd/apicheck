

skip_if_mran_down <- function () {
  prepare <- try(pastapi:::cached_install("assertthat", "0.2.0"), silent = TRUE)
  if (class(prepare) == "try-error") skip("Couldn't download assertthat, MRAN may be down")
}
