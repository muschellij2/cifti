#' @title Download CIFTI Test Data
#' @description Downloads CIFTI test data from
#' \url{https://www.nitrc.org/frs/download.php/8541/cifti-2_test_data-1.2.zip}
#' @param outdir Output directory for test file directory
#' @param overwrite Should files be overwritten if already exist?
#' @param ... additional arguments to \code{\link{download.file}}
#'
#' @return Vector of file names
#' @export
#'
#' @importFrom utils download.file unzip
download_cifti_data = function(
  outdir = system.file(package = "cifti"),
  overwrite = FALSE,
  ...
  ) {
  # simple workaround for vignettes
  if (missing(outdir)) {
    if (outdir == "") {
      outdir = tempdir()
    }
  }
  expected_files = c("ones.dscalar.nii",
                     "Conte69.MyelinAndCorrThickness.32k_fs_LR.dscalar.nii",
                     "Conte69.R.inflated.32k_fs_LR.surf.gii",
                     "Conte69.L.inflated.32k_fs_LR.surf.gii",
                     "Conte69_AverageT1w_restore.nii.gz",
                     "Conte69.MyelinAndCorrThickness.32k_fs_LR.ptseries.nii",
                     "Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii",
                     "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii")
  expected_files = file.path("cifti-2_test_data", expected_files)
  out_files = file.path(outdir, expected_files)
  if (!all(file.exists(out_files)) || overwrite) {
    url = paste0( "https://www.nitrc.org/frs/download.php",
                  "/8541/cifti-2_test_data-1.2.zip")
    destfile = basename(url)
    destfile = file.path(outdir, destfile)
    download.file( url = url, destfile = destfile)
    out_files = unzip(destfile, exdir = outdir, files = expected_files,
                      overwrite = overwrite)
    file.remove(destfile)
  }
  return(out_files)
}

#' @title Check Presence of CIFTI Test Data
#' @description Checks if CIFTI test data is downloaded
#'
#' @param outdir Output directory for test file directory
#'
#' @return Logical indicator
#' @export
have_cifti_test_data = function(
  outdir = system.file(package = "cifti")
  ) {
  # simple workaround for vignettes
  if (missing(outdir)) {
    if (outdir == "") {
      outdir = tempdir()
    }
  }
  expected_files = c("ones.dscalar.nii",
                     "Conte69.MyelinAndCorrThickness.32k_fs_LR.dscalar.nii",
                     "Conte69.R.inflated.32k_fs_LR.surf.gii",
                     "Conte69.L.inflated.32k_fs_LR.surf.gii",
                     "Conte69_AverageT1w_restore.nii.gz",
                     "Conte69.MyelinAndCorrThickness.32k_fs_LR.ptseries.nii",
                     "Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii",
                     "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii")
  expected_files = file.path("cifti-2_test_data", expected_files)
  out_files = file.path(outdir, expected_files)
  res = all(file.exists(out_files))
  return(res)
}