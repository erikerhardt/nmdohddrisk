## Startup functions ------------------------------------

# Based on zzz.R from the tidyverse package, https://github.com/tidyverse/tidyverse/blob/main/R/zzz.R
#' .onAttach start message
#'
#' @param ...             arguments
#' @return invisible(NULL)
.onAttach <- function(...) {

  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()

  crayon::num_colors(TRUE)
  nmdohddrisk_attach()

  if (!"package:conflicted" %in% search()) {
    x <- nmdohddrisk_conflicts()
    msg(nmdohddrisk_conflict_message(x), startup = TRUE)
  }

  start_message <- c( "nmdohddrisk, Risk Prediction model for NM DOH DD ANE\n"
                      , "  by Dr. Erik Barry Erhardt <erik@StatAcumen.com>")
  packageStartupMessage(start_message)
  #print(nmdohddrisk_logo())

  invisible(NULL)
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

#' .onLoad getOption package settings
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return invisible(NULL)
#'
#' @examples
#' getOption("nmdohddrisk.name")
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.nmdohddrisk <- list(
    #nmdohddrisk.path = "~/R-dev",
    nmdohddrisk.install.args  = "",
    nmdohddrisk.name          = "Erik Barry Erhardt",
    nmdohddrisk.desc.author   = "Erik Erhardt <erik@StatAcumen.com> [aut, cre]",
    nmdohddrisk.desc.license  = "GPL (>= 2)",
    nmdohddrisk.desc.suggests = NULL,
    nmdohddrisk.desc          = list()
  )
  toset <- !(names(op.nmdohddrisk) %in% names(op))
  if(any(toset)) options(op.nmdohddrisk[toset])

  invisible(NULL)
}
