TODO
* deal with S4 methods; and test for that.
* tests: maybe rather than pre-installed (OS X) versions, have pre-installed source
  files and then mock install.versions
* add function to check incremental burden of a dependency?
* BUG: compare_versions fails silently when installed library is used (namespace unloading?)
* BUG: compare_versions seems to overreport API changes, e.g. try `reprex`
  - One underlying reason is that `x <- loadNamespace(..., partial = TRUE)``
    can result in no exports from `getNamespaceExports(x)`; but not if the namespace has
    previously been loaded with partial = FALSE. And this applies even if the namespaces
    are separate objects.
  - Be aware especially that if the user has previously loaded a package (even indirectly
    via a dependency) then getNamespaceExports will return stuff from partial = TRUE; if
    not, not.
  - Relevant example:
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
Clean unloading and reloading of current packages; always leave the computer in the state it was in before
