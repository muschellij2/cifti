#' @title Parse Surface from CIFTI
#' @description Extracts information about Surfaces from CIFTI file
#' @param nodeset Set of XML nodes corresponding to \code{Surface}
#'
#' @return List of values
#' @export
#'
#' @examples \dontrun{
#' doc = cifti_xml(fname)
#' nodes = xml_find_all(doc, "/CIFTI/Matrix/MatrixIndicesMap")
#' nodeset = xml_find_all(nodes, "./Surface")
#' parse_volume(nodeset)
#' }
parse_surface = function(nodeset) {
  n_nodes = length(nodeset)
  all_attrs = xml_attrs(nodeset)
  all_attrs = lapply(all_attrs, as.list)
  all_attrs = lapply(all_attrs, function(x) {
    nums = is_cifti_numeric(names(x))
    if (any(nums)) {
      x[nums] = lapply(x[nums], as.numeric)
    }
    return(x)
  })
  return(all_attrs)
}


#' @rdname parse_surface
#' @param fname filename of CIFTI file
#' @export
get_surface = function(fname) {
  nodes = matrix_ind_map_nodes(fname)
  nodeset = xml_find_all(nodes, "./Surface")
  parse_surface(nodeset)
}