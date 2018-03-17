
# these functions should have nothing to do with versioning


is_api_same <- function (fun1, fun2) {
  identical(formals(fun1), formals(fun2))
}


obj_exists_in_ns <- function (nm, ns) {
  # this includes everything
  # weaker would be ls("package:ns_name") which includes only exports;
  # intermediate is ls("package:ns_name", all.names = TRUE) which includes stuff with a "."
  # like the S3 methods table
  nm %in% names(ns)
}

