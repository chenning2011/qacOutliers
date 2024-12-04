.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages({
    #NO CONLFLICT WARNINGS
    suppressPackageStartupMessages(library(gridExtra, warn.conflicts = FALSE, quietly = TRUE))
    suppressPackageStartupMessages(library(stats, warn.conflicts = FALSE, quietly = TRUE))
    suppressPackageStartupMessages(library(dplyr, warn.conflicts = FALSE, quietly = TRUE))
    suppressPackageStartupMessages(library(dbscan, warn.conflicts = FALSE, quietly = TRUE))
  }))
}
