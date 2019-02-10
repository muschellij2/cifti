#' @title Read NIfTI-2 Header
#' @description Reads a NIfTI-2 header from a filename
#' @param fname Filename
#' @param verbose Print diagnostic messages
#' @param warn Should warnings be printed? Passed to \code{\link{options}}
#'
#' @return Object of class \code{nifti}
#' @export
#' @note The \code{unused_str} part of the header is not returned, but is an
#' empty string of 15 characters.  This code was adapted by
#' the \code{oro.nifti} package
#' @importFrom oro.nifti nifti
nifti_2_hdr = function(fname, verbose = FALSE, warn = -1) {
  ## Open appropriate file
  fid <- file(fname, "rb")
  if (verbose) {
    cat("  hdr   =", fname, fill = TRUE)
  }
  ## Warnings?
  oldwarn <- getOption("warn")
  options(warn = warn)
  on.exit({
    close(fid)
    options(warn = oldwarn)
  })
  ## Test for endian properties
  endian <- .Platform$endian
  sizeof.hdr <- readBin(fid, integer(), size = 4, endian = endian)
  if (sizeof.hdr != 540) {
    close(fid)
    stop("Header size is not 540 - likely not a NIfTI-2 or CIFTI file!")
  }
  ## Construct S4 object
  #int32 is size 4
  #int16 is size 2

  nim <- oro.nifti::nifti()
  nim@"sizeof_hdr" <- sizeof.hdr
  nim@"magic" <- cifti_read_char(fid, n=8)
  nim@"datatype" <- readBin(fid, integer(), size=2,
                            endian=endian)
  nim@"bitpix" <- readBin(fid, integer(), size=2,
                          endian=endian)

  # worked for int64
  nim@"dim_" <- readBin(fid, integer(), n = 8,
                        size = 8, endian=endian)

  nim@"intent_p1" <- readBin(fid, double(),
                             size=8, endian=endian)
  nim@"intent_p2" <- readBin(fid, double(),
                             size=8, endian=endian)
  nim@"intent_p3" <- readBin(fid, double(),
                             size=8, endian=endian)
  nim@"pixdim" <- readBin(fid, double(), 8,
                          size=8, endian=endian)
  nim@"vox_offset" <- readBin(fid, integer(),
                              size = 8, endian=endian)
  if (nim@"vox_offset" < 544) {
    warning("vox_offset seems off!")
  }
  if (verbose) {
    cat("  vox_offset =", nim@"vox_offset", fill=TRUE)
  }
  nim@"scl_slope" <- readBin(fid, double(),
                             size=8, endian=endian)
  nim@"scl_inter" <- readBin(fid, double(),
                             size=8, endian=endian)
  nim@"cal_max" <- readBin(fid, double(),
                           size=8, endian=endian)
  nim@"cal_min" <- readBin(fid, double(),
                           size=8, endian=endian)

  nim@"slice_duration" <- readBin(fid, double(),
                                  size=8, endian=endian)
  nim@"toffset" <- readBin(fid, double(),
                           size=8, endian=endian)

  nim@"slice_start" <- readBin(fid, integer(),
                               size=8, endian=endian)
  nim@"slice_end" <- readBin(fid, integer(),
                             size=8, endian=endian)
  nim@"descrip" <- cifti_read_char(fid,
                                   n=80)
  nim@"aux_file" <- cifti_read_char(fid,
                                    n=24)

  nim@"qform_code" <- readBin(fid, integer(), size=4, endian=endian)
  nim@"sform_code" <- readBin(fid, integer(), size=4, endian=endian)
  nim@"quatern_b" <- readBin(fid, double(), size=8, endian=endian)
  nim@"quatern_c" <- readBin(fid, double(), size=8, endian=endian)
  nim@"quatern_d" <- readBin(fid, double(), size=8, endian=endian)
  nim@"qoffset_x" <- readBin(fid, double(), size=8, endian=endian)
  nim@"qoffset_y" <- readBin(fid, double(), size=8, endian=endian)
  nim@"qoffset_z" <- readBin(fid, double(), size=8, endian=endian)
  nim@"srow_x" <- readBin(fid, double(), 4, size=8, endian=endian)
  nim@"srow_y" <- readBin(fid, double(), 4, size=8, endian=endian)
  nim@"srow_z" <- readBin(fid, double(), 4, size=8, endian=endian)

  # nim@"slice_code" <- readBin(fid, integer(),
  #                             size=4, signed=FALSE,
  #                             endian=endian)
  # nim@"xyzt_units" <- readBin(fid, integer(),
  #                             size=4, signed=FALSE, endian=endian)
  # nim@"intent_code" = readBin(fid, integer(),
  #                             size=4, signed=FALSE, endian=endian)
  nim@"slice_code" <- readBin(fid, integer(),
                              size = 4,
                              endian = endian)
  nim@"xyzt_units" <- readBin(fid, integer(),
                              size = 4,
                              endian = endian)
  nim@"intent_code" = readBin(fid, integer(),
                              size = 4,
                              endian = endian)
  nim@"intent_name" <- cifti_read_char(fid,
                                       n=16)
  nim@"dim_info" <- cifti_read_char(fid,
                                    n=1)
  # txt <- readBin(fid, "raw", n)
  unused_str = cifti_read_char(fid,
                               n=15)
  nhdr = seek(fid)
  stopifnot(nhdr == sizeof.hdr)

  return(nim)
}