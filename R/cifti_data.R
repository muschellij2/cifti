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
  vals = array(vals, dim = cifti_dim)
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