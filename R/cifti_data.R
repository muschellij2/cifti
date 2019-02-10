#' @title Extract CIFTI data from file
#' @description Extracts the data after the CIFTI XML information
#' @param fname Filename of CIFTI
#' @param nim NIfTI-2 header, if already parsed.
#' If \code{NULL}, \code{\link{nifti_2_hdr}}
#' will be run on the CIFTI.
#'
#' @return Array of values
#' @export
cifti_data = function(fname, nim = NULL) {
  if (is.null(nim)) {
    nim = nifti_2_hdr(fname = fname)
  }
  fid = file(fname, "rb")
  on.exit({
    close(fid)
  })
  seek(fid, where = 0, origin = "end")
  filesize = seek(fid)
  seek(fid, where = nim@vox_offset, origin = "start");

  dtype = as.character(nim@datatype)
  img_dim = nim@dim_
  n_items = prod(img_dim[2:length(img_dim)])

  what = switch(dtype,
                "2" = integer(),
                "4" = numeric(),
                "8" = integer(),
                "16" = numeric(),
                "64" = double(),
                "512" = numeric(),
                "768" = integer()
  )
  size = switch(dtype,
                "2" = 1,
                "4" = 2,
                "8" = 4,
                "16" = 4,
                "64" = 8,
                "512" = 2,
                "768" = 4
  )
  if (is.null(what) || is.null(size)) {
    stop("Unsupported data type indicated by NIfTI-2 header!")
  }
  vals = readBin(con = fid,
                 what = what,
                 n = filesize * 2, size = size)
  if (n_items > length(vals)) {
    stop("Not all CIFTI data read!")
  }
  if (n_items < length(vals)) {
    stop("More data read than header indicated - header or data problem!")
  }

  cifti_dim = img_dim[6:8]
  ## === myc 20190207
  # some cifti (e.g., MSC) has dense data (60k+ vertex) comes first:
  #      e.g., Dimension       : 1 x 1 x 1 x 1 x 65890 x 818
  #      Whereas standard cifti has rows * dense for dtseries https://www.humanconnectome.org/software/workbench-command/-cifti-help
  if (cifti_dim[1] > cifti_dim[2]) {
    temp_vals = array(vals, dim = img_dim[c(7,6,8)])               # fill array based on dense coming first
    vals = array(t(as.matrix(temp_vals[,,])), dim = cifti_dim)     # transform array back to origianl dimension to match header
  } else{
    vals = array(vals, dim = cifti_dim)
  }

  # [m, n] = size(voxdata);
  # if m>n
  # dat = nan(Ngreynodes,n);
  # dat(greynodeIndex(dataIndex),:) = voxdata;
  # else
  #   dat = nan(Ngreynodes,m);
  # dat(greynodeIndex(dataIndex),:) = transpose(voxdata);
  # end
  return(vals)
}