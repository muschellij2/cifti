#' @title Extract MatrixIndicesMap nodes from CIFTI
#' @description Extracts the nodes from a CIFTI-2 file corresponding to the
#' \code{MatrixIndicesMap} branch
#'
#' @param fname File of CIFTI data
#'
#' @return Nodes of class \code{xml_nodeset}
#' @export
#' @importFrom xml2 xml_find_all
matrix_ind_map_nodes = function(fname) {
  doc = cifti_xml(fname)
  nodes = xml_find_all(doc, "/CIFTI/Matrix/MatrixIndicesMap")
}