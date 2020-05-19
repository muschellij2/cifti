#' @title Parse Parcel from CIFTI
#' @description Extracts information about Parcels from CIFTI file
#' @param nodeset Set of XML nodes corresponding to \code{Parcel}
#' @param verbose print diagnostic messages
#'
#' @return List of values
#' @export
parse_parcel = function(nodeset) {
  if (is.list(nodeset) &&
      !inherits(nodeset, "xml_nodes") &&
      !inherits(nodeset, "xml_nodeset")) {
    return(lapply(nodeset, parse_parcel))
  }
  n_nodes = length(nodeset)
  all_names = xml_attrs(nodeset, "Name")
  all_names = unlist(all_names)

  vert_nodes = lapply(nodeset,
                      xml_find_all,
                      xpath = "./Vertices")
  return_verts = any(sapply(vert_nodes, length) > 0)

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


  vind = lapply(nodeset,
                xml_find_all,
                xpath = "./VoxelIndicesIJK")
  return_vind = any(sapply(vind, length) > 0)

  # i = 1
  get_vind = function(node) {
    verts = xml_text(node)
    if (length(verts) > 0) {

      verts = lapply(
        strsplit(verts, split = "\n"),
        function(x) {
          x = strsplit(x, split= " ")
          x = lapply(x, as.numeric)
          do.call(rbind, x)
        })
      if (length(verts) == 1) {
        verts = verts[[1]]
      } else {
        verts = do.call(rbind, verts)
      }
      if (ncol(verts) == 3) {
        colnames(verts) = c("I", "J", "K")
      } else {
        warning("VoxelInidices shoul have 3 columns!")
      }
      verts
    }
    return(verts)
  }
  vind = lapply(vind, get_vind)
  names(vind) = all_names
  L = list(vertices = verts, indices = vind)
  if (return_vind & return_verts) {
    warning(
      paste0(
        "Please send this case to ",
        "https://github.com/muschellij2/cifti/issues, ",
        "returning a list of indices and vertices"
      )
    )
    return(L)
  }
  if (!return_vind & !return_verts) {
    return(L)
  }
  if (!return_verts) {
    L$vertices = NULL
  }
  if (!return_vind) {
    L$indices = NULL
  }
  if (length(L) == 1) {
    L = L[[1]]
  }
  return(L)
}

#' @rdname parse_parcel
#' @param fname filename of CIFTI file
#' @export
get_parcel = function(fname, verbose = TRUE) {
  nodes = matrix_ind_map_nodes(fname)
  nodeset = lapply(nodes, xml_find_all, xpath = "./Parcel")
  nodeset = keep_sub_nodeset(nodeset)
  if (verbose) {
    message("Parsing Parcel Data")
  }
  parse_parcel(nodeset)
}