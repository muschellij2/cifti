#' @title Extract Brain Structures from CIFTI Brain Models
#' @description Extracts the \code{BrainStructure} attribute from a
#' \code{BrainModel} in a \code{cifti} object
#'
#' @param file \code{cifti} object
#'
#' @return A vector of brain structure names
#' @export
cifti_brain_structs = function(file) {
  if (is.cifti(file)) {
    cii = file
  } else {
    cii = read_cifti(file)
  }
  struct_names = sapply(cii$BrainModel, attr, "BrainStructure")
  return(struct_names)
}