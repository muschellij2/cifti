#' @importFrom utils citation
.onAttach <- function(...) {
  if (!interactive()) return()

  package_name = "cifti"
  suppressWarnings({
    x = citation(package_name)
  })
  if (is.null(x[[1]]$year)) {
    cc = class(x)
    x = unclass(x)
    x[[1]]$year = format(Sys.Date(), "%Y")
    class(x) = cc
  }
  x = format(x, "text")
  x = paste(x, collapse = "\n\n")
  ack <- c(
    paste0("Please cite the ", package_name,
           " package using:\n"),
    x)

  packageStartupMessage(paste(strwrap(ack), collapse = "\n"))
}