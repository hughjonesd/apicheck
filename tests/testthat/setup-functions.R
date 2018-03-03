

skip_if_mran_down <- memoise::memoise( function () {
  # small 17K package
  tempdir <- tempfile(pattern = "testing", tmpdir = normalizePath(tempdir()))
  dir.create(tempdir)
  prepare <- try(versions::install.versions("enrichwith", "0.0.3", lib = tempdir), silent = TRUE)
  if (class(prepare) == "try-error") skip("Couldn't download test package, MRAN may be down")
})
