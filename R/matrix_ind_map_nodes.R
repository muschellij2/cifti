#' @title Extract MatrixIndicesMap nodes from CIFTI
#' @description Extracts the nodes from a CIFTI-2 file corresponding to the
#' \code{MatrixIndicesMap} branch
#'
#' @param fname File of CIFTI data
#'
#' @return Nodes of class \code{xml_nodeset}
#' @export
#' @importFrom xml2 read_xml xml_find_all
matrix_ind_map_nodes = function(fname) {
  xmldata = cifti_xml(fname)
  doc = read_xml(xmldata)
  nodes = xml_find_all(doc, "/CIFTI/Matrix/MatrixIndicesMap")
}