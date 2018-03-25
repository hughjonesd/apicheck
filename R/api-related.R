
# these functions should have nothing to do with versioning

get_current_ns <- function (package) {
  ns <- tryCatch(loadNamespace(package, partial = TRUE), error = function (e) {
    stop("Couldn't load current version of package.\n",
      "Do you have it installed? If not run `install.packages('", package, "')`.\n",
      "Or, use `current_fun = fun_at(fun, package, version)` to compare to a version\n",
      " without doing a full install.\n",
      "Original error:", e$message, call. = FALSE)
  })
  unload_noncore_namespace(package)

  return(ns)
}


is_api_same <- function (fun1, fun2) {
  # we use args because it can cope with Primitive functions, see ?formals
  identical(formals(args(fun1)), formals(args(fun2)))
}


get_fun_in_ns <- function (fun, ns) {
  tryCatch({
      x <- if (utils::isS3method(fun, envir = ns)) {
        bits <- strsplit(fun, ".", fixed = TRUE)[[1]]
        generic_fun <- paste(bits[ -length(bits) ], collapse = ".")
        class <- bits[length(bits)]
        utils::getS3method(generic_fun, class, envir = ns)
      } else {
        get(fun, ns, inherits = FALSE)
      }
    },
    # a uniform error message can be caught upstream
    error = function (e) stop_fun_not_found(fun, ns)
  )

  return(x)
}


get_stub_fun_in_core <- function (fun, package, version) {
  stopifnot(is_core_package(package))

  rch <- get_rcheology_rows(name = fun, package = package) # memoised for speed
  rch <- rch[rch$Rversion == version,]
  if (nrow(rch) == 0L) stop_fun_not_found(fun, package, version)
  stopifnot(nrow(rch) == 1L)
  fun_args <- rch$args
  if (is.na(fun_args)) stop("Could not determine arguments of `", fun, "` in rcheology database of core R functions")
  fake_fun <- paste("function", fun_args, "NULL")
  fake_fun <- eval(parse(text = fake_fun))

  return (fake_fun)
}

core_packages <- function () {
  ip <- as.data.frame(utils::installed.packages())
  ip <- dplyr::filter(ip, Priority == "base")
  as.character(ip$Package)
}


is_core_package <- memoise::memoise( function (package) {
  return(package %in% core_packages())
})

# used elsewhere to check for this error
FUNCTION_NOT_FOUND <- "Could not find function in namespace"


stop_fun_not_found <- function (fun, ns_or_package, version) {
  if (is.environment(ns_or_package)) {
    package <- getNamespaceName(ns_or_package)
    version <- getNamespaceVersion(ns_or_package)
  } else {
    package <- ns_or_package
  }
  stop(FUNCTION_NOT_FOUND, ": ", fun, " in ", package, " version ", version)
}

