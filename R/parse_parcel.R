#' @title Parse Parcel from CIFTI
#' @description Extracts information about Parcels from CIFTI file
#' @param nodeset Set of XML nodes corresponding to \code{Parcel}
#'
#' @return List of values
#' @export
parse_parcel = function(nodeset) {
  n_nodes = length(nodeset)
  all_names = xml_attrs(nodeset, "Name")
  all_names = unlist(all_names)

  vert_nodes = lapply(nodeset,
                      xml_find_all,
                      xpath = "./Vertices")

  # i = 1
  get_verts = function(node) {
    # print(i)
    # i <<- i + 1
    vert_attr = xml_attrs(node)
    if (length(vert_attr) > 1){
      vert_attr = lapply(vert_attr,
                         as.list)
    } else {
      if (length(vert_attr) == 0){
        vert_attr = NULL
      }
      vert_attr = as.list(vert_attr[1])
    }
    verts = xml_text(node)
    if (length(verts) > 0) {

      verts = lapply(verts, strsplit,
                     split = " ")
      verts = lapply(verts, `[[`, 1)
      verts = lapply(verts, as.numeric)
      verts = mapply(function(x, a){
        attributes(x) = as.list(a)
        return(x)
      }, verts, vert_attr, SIMPLIFY = FALSE)
    }
    return(verts)
  }
  verts = lapply(vert_nodes, get_verts)
  names(verts) = all_names
  return(verts)
}

#' @rdname parse_parcel
#' @param fname filename of CIFTI file
#' @export
get_parcel = function(fname) {
  nodes = matrix_ind_map_nodes(fname)
  nodeset = xml_find_all(nodes, "./Parcel")
  parse_parcel(nodeset)
}