#' @title Test if CIFTI
#' @description Simple wrapper to determine if class is CIFTI
#'
#' @param x object to test
#'
#' @return Logical if \code{x} is CIFTI
#' @export
is.cifti = function(x) {
  inherits(x, "cifti")
}