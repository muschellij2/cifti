#' @title Read CIFTI File
#' @description Reads CIFTI Files
#' @param fname file name of CIFTI file
#' @param drop_data Should the empty data dimensions be dropped?
#' @param trans_data Should the data be transposed
#'
#' @return List of information from the CIFTI file
#' @export
#' @examples
#' if (have_cifti_test_data()) {
#'    files = download_cifti_data()
#'    fname = grep("MyelinAndCorrThickness.32k_fs_LR.dscalar",
#'    files, value = TRUE)
#'    res = read_cifti(fname)
#' }
read_cifti = function(fname,
                      drop_data = TRUE,
                      trans_data = TRUE) {
  res = get_cifti_type(fname)
  data = cifti_data(fname)

  #########################################
  # Dropping empty dimensions
  #########################################
  if (drop_data) {
    data = drop(data)
  }
  attr(data, "drop") = drop_data

  #########################################
  # Transposing data
  #########################################
  ddata = dim(data)
  if (length(ddata) == 2) {
    if (trans_data) {
      data = t(data)
    } else {
      warning("Dimensions of the data > 2, so no transposing done!")
    }
  }
  attr(data, "trans") = trans_data

  res$data = data
  hdr = nifti_2_hdr(fname)
  res$hdr = hdr

  return(res)
}

#' @rdname read_cifti
#' @export
readCIFTI = function(fname,
                      drop_data = TRUE,
                      trans_data = TRUE) {
  res = read_cifti(fname = fname,
             drop_data = drop_data,
             trans_data = trans_data)
  return(res)
}