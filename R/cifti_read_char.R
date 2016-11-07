#' @title Read characters with embedded nulls
#' @description Simple wrapper for reading in character values with embedded nulls in a
#' binary file
#' @param fid identifier of the open file connection
#' @param n number of elements to read
#' @param to A character string describing the target encoding.
#'
#' @return Character vector
#' @export
cifti_read_char = function(fid, n, to = "UTF-8") {
  txt <- readBin(con = fid, what = "raw", n = n)
  iconv(rawToChar(txt[txt != as.raw(0)]), to = to)
}
