
# these functions should have nothing to do with versioning

get_current_ns <- function (package) {
  ns <- tryCatch(loadNamespace(package, partial = TRUE), error = function (e) {
    stop("Couldn't load current version of package.\n",
      "Do you have it installed? If not run `install.packages('", package, "')`.\n",
      "Or, use `current_fun = fun_at(fun, package, version)` to compare to a version\n",
      " without doing a full install.\n",
      "Original error:", e$message, call. = FALSE)
  })
  unloadNamespace(package)

  return(ns)
}


is_api_same <- function (fun1, fun2) {
  identical(formals(fun1), formals(fun2))
}


# here fun could be a S3 method
get_fun_in_ns <- function (fun, ns) {
  x <- if (utils::isS3method(fun, envir = ns)) {
    bits <- strsplit(fun, ".", fixed = TRUE)[[1]]
    generic_fun <- paste(bits[-length(bits)], collapse = ".")
    class <- bits[length(bits)]
    utils::getS3method(generic_fun, class, envir = ns)
  } else {
    get(fun, ns)
  }

  return(x)
}

