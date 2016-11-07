parse_parcel = function(nodeset) {
  n_nodes = length(nodeset)
  all_names = xml_attrs(nodeset, "Name")
  all_names = unlist(all_names)

  vert_nodes = lapply(nodeset,
                      xml_find_all,
                      xpath = "./Vertices")

  get_verts = function(node) {
    vert_attr = xml_attrs(node)
    if (length(vert_attr) > 1){
      vert_attr = lapply(vert_attr,
                         as.list)
    } else {
      if (length(vert_attr) == 0){
        vert_attr = NULL
      }
      vert_attr = as.list(vert_attr[[1]])
    }
    verts = xml_text(node)
    if (length(verts) > 0) {

      verts = lapply(verts, strsplit,
                     split = " ")
      verts = lapply(verts, `[[`, 1)
      verts = lapply(verts, as.numeric)
      verts = mapply(function(x, a){
        attributes(x) = a
        return(x)
      }, verts, vert_attr, SIMPLIFY = FALSE)
    }
    return(verts)
  }
  verts = lapply(vert_nodes, get_verts)
  names(verts) = all_names
  return(verts)
}