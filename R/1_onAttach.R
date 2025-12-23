#' @import data.table
.onAttach <- function(libname, pkgname) {
  version <- tryCatch(
    utils::packageDescription("csdata", fields = "Version"),
    warning = function(w){
      1
    }
  )

  packageStartupMessage(paste0(
    "csdata ",
    version,
    "\n",
    "https://niphr.github.io/csdata/"
  ))
}
