#' @title Parse Volume from CIFTI
#' @description Extracts information about Volumes from CIFTI file
#' @param nodeset Set of XML nodes corresponding to \code{Volume}
#'
#' @return List of values
#' @export
#'
#' @examples \dontrun{
#' doc = cifti_xml(fname)
#' nodes = xml_find_all(doc, "/CIFTI/Matrix/MatrixIndicesMap")
#' nodeset = xml_find_all(nodes, "./Volume")
#' parse_volume(nodeset)
#' }
parse_volume = function(nodeset) {
  n_nodes = length(nodeset)
  all_names = xml_attrs(nodeset,
                        "VolumeDimensions")
  all_names = unlist(all_names)

  vert_nodes = lapply(nodeset,
                      xml_find_all,
                      xpath = "./TransformationMatrixVoxelIndicesIJKtoXYZ")

  get_mat = function(node) {
    vert_attr = xml_attrs(node)
    if (length(vert_attr) > 1){
      vert_attr = lapply(vert_attr,
                         as.list)
    } else {
      if (length(vert_attr) == 0) {
        vert_attr = NULL
      }
      vert_attr = as.list(vert_attr[[1]])
    }
    verts = xml_text(node)
    if (length(verts) > 0) {
      verts = strsplit(verts, split = "\n")[[1]]
      verts = verts[ !(verts %in% "")]
      verts = strsplit(verts, " ")
      verts = lapply(verts, as.numeric)
      verts = do.call("rbind", verts)
      # attributes(vert) = vert_attr
    }
    return(verts)
  }
  verts = lapply(vert_nodes, get_mat)
  verts = mapply(function(x, a) {
    list(mat = x, VolumeDimensions = a)
  }, verts, all_names, SIMPLIFY = FALSE)
  if (length(verts) == 1) {
    verts = verts[[1]]
  }
  return(verts)
}


#' @rdname parse_volume
#' @param fname filename of CIFTI file
#' @export
get_volume = function(fname) {
  nodes = matrix_ind_map_nodes(fname)
  nodeset = xml_find_all(nodes, "./Volume")
  parse_volume(nodeset)
}