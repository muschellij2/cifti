
#' @title Extract CIFTI XML
#' @description Extracts CIFTI XML from the CIFTI file
#' @param fname filename of CIFTI
#' @param nim NIfTI-2 header, if already parsed.
#' If \code{NULL}, \code{\link{nifti_2_hdr}}
#' will be run on the CIFTI.
#'
#' @return Character string of XML information
#' @export
cifti_xml = function(fname, nim = NULL){
  if (is.null(nim)) {
    nim = nifti_2_hdr(fname = fname)
  }
  fid = file(fname, "rb")
  on.exit({
    close(fid)
  })
  seek(fid, where = 0, origin = "end")
  filesize = seek(fid)
  seek(fid, where = 540, origin = "start");

  add_hdr = readBin(fid, what = integer(), n = 4,
                    size = 1)

  esize = readBin(fid,
                  what = integer(),
                  size = 4)
  etype = readBin(fid,
                  what = integer(),
                  size = 4)


  hdrsize = 540;
  voxsize = filesize - nim@vox_offset;
  if ( esize > filesize - hdrsize - voxsize ) {
    stop(paste0("the endianness of the header ",
                "extension is inconsistent with the nifti-2 ",
                "header"))
  }

  xmldata = cifti_read_char(fid,
                            n = esize - 8)

  return(xmldata)
}