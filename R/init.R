# from: https://github.com/cran/sf/blob/master/R/tidyverse.R:
# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R
# Thu Apr 19 10:53:24 CEST 2018
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

register_all_s3_methods <- function() {

  # tidyverse joins
  register_s3_method("dplyr", "inner_join", "ir")
  register_s3_method("dplyr", "left_join", "ir")
  register_s3_method("dplyr", "right_join", "ir")
  register_s3_method("dplyr", "full_join", "ir")
  register_s3_method("dplyr", "semi_join", "ir")
  register_s3_method("dplyr", "anti_join", "ir")

  register_s3_method("dplyr", "filter", "ir")
  register_s3_method("dplyr", "arrange", "ir")
  register_s3_method("dplyr", "distinct", "ir")
  register_s3_method("dplyr", "group_by", "ir")
  register_s3_method("dplyr", "mutate", "ir")
  register_s3_method("dplyr", "rename", "ir")
  register_s3_method("dplyr", "rename_with", "ir")
  register_s3_method("dplyr", "rowwise", "ir")
  register_s3_method("dplyr", "select", "ir")
  register_s3_method("dplyr", "slice", "ir")
  # register_s3_method("dplyr", "slice_head", "ir")
  # register_s3_method("dplyr", "slice_tail", "ir")
  # register_s3_method("dplyr", "slice_min", "ir")
  # register_s3_method("dplyr", "slice_max", "ir")
  register_s3_method("dplyr", "slice_sample", "ir")
  register_s3_method("dplyr", "summarise", "ir")
  register_s3_method("dplyr", "summarize", "ir")
  register_s3_method("dplyr", "transmute", "ir")
  register_s3_method("dplyr", "ungroup", "ir")
  register_s3_method("tidyr", "pivot_longer", "ir")
  register_s3_method("tidyr", "nest", "ir")
  register_s3_method("tidyr", "unnest", "ir")
  register_s3_method("tidyr", "separate", "ir")
  register_s3_method("tidyr", "separate_rows", "ir")
  register_s3_method("tidyr", "unite", "ir")

}

.onLoad <- function(libname, pkgname) {

  register_all_s3_methods()

}
