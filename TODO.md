TODO

* deal with S4 methods; and test for that.
* tests: maybe rather than pre-installed (OS X) versions, have pre-installed source
  files and then mock install.versions
* BUG: can't unload dependencies of apicheck itself, causing problems when checking tidyverse packages
* BUG: in console, with quiet = TRUE, you don't get asked to select CRAN mirror
* Should compare_versions (optionally) report about unexported methods, e.g. crayon::start.crayon?
  - Note that e.g. `get_fun_in_ns('start.crayon', getNamespace('crayon'))` will work;
  - Possibly `fun_names_in_ns` should report the same.
* Check that partial defaults are sensible. Consider the following:
  
```
> ns <- loadNamespace("clipr", partial = TRUE)
> getNamespaceExports(ns)
character(0)
> ns2 <- loadNamespace("clipr", partial = FALSE)
> getNamespaceExports(ns)
character(0)
> getNamespaceExports(ns2)
[1] "clipr_available" "write_clip"      "clear_clip"      "read_clip_tbl"   "dr_clipr"
[6] "read_clip"
> ns3 <- loadNamespace("clipr", partial = FALSE)
> getNamespaceExports(ns3)
[1] "clipr_available" "write_clip"      "clear_clip"      "read_clip_tbl"   "dr_clipr"
[6] "read_clip"
> identical(ns, ns3)
[1] FALSE
> identical(ns2, ns3)
[1] TRUE
```
