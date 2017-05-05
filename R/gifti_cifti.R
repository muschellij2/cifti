#' @title Map CIFTI values to Coordinates using GIFTI
#' @description Maps the data portion of CIFTI data set from a
#' Brain Model to the xyz coordinate triangles
#' @param file filename of CIFTI file or \code{cifti} object
#' @param gii_file filename of corresponding
#' GIFTI file or \code{gifti} object
#' @param structure Structure to map, must be one of the brain models in
#' the CIFTI
#' @param add_one should 1 be added to indices (1-based vs. 0-based)
#'
#' @return List of coordinates and values
#' @importFrom gifti is.gifti gifti_map_value readgii
#' @export
cifti_coords_gifti = function(
  file,
  gii_file,
  structure,
  add_one = TRUE) {
  if (is.cifti(file)) {
    cii = file
  } else {
    cii = read_cifti(file)
  }
  if (is.gifti(gii_file)) {
    gii = gii_file
  } else {
    gii = readgii(gii_file)
  }

  bs = cifti_brain_structs(cii)
  if (!structure %in% bs) {
    stop(paste0(structure, " not in CIFTI brain structures.  ",
                "Those available: ", paste(bs, collapse = ", "))
    )
  }
  names(cii$BrainModel) = bs

  # grabbing the data
  values = as.matrix(cii$data)
  indices = cii$BrainModel[[structure]] + add_one
  values = values[seq(indices),, drop = FALSE]

  vals = apply(values, 2,
               function(x) {
                 gifti::gifti_map_value(
                   pointset = gii$data$pointset,
                   triangle = gii$data$triangle,
                   values = x,
                   indices = indices)
               })
  if (length(vals) == 1) {
    vals = vals[[1]]
  }
  return(vals)
}