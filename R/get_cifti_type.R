#' @title Get Generic CIFTI Type
#' @description Wrapper for multiple types of CIFTI XML types.
#' @param fname File name of CIFTI file
#' @param type type of data to extract from CIFTI XML
#' @param verbose print diagnostic messages
#'
#' @return List of output from each type
#' @export
#' @importFrom R.utils gunzip
#' @importFrom xml2 as_list
get_cifti_type = function(
  fname,
  type = c("Volume", "Surface",
           "Parcel",  "NamedMap",
           "BrainModel"),
  verbose = TRUE) {
  type = match.arg(type, several.ok = TRUE)
  func_name = function(run_type) {
    run_type = tolower(run_type)
    run_type[ run_type %in% "brainmodel" ] = "brain_model"
    run_type[ run_type %in% "namedmap" ] = "named_map"
    paste0("get_", run_type)
  }
  funcs = func_name(type)

  args = list(fname = fname)
  args$verbose = verbose
  res = lapply(funcs, do.call, args = args)
  names(res) = type
  L = sapply(res, length)
  res = res[L > 0]
  return(res)

}

#' @rdname get_cifti_type
#' @export
cifti_as_list = function(
  fname,
  type = c("Volume", "Surface",
           "Parcel",  "NamedMap",
           "BrainModel")) {
  type = match.arg(type, several.ok = TRUE)

  nodes = matrix_ind_map_nodes(fname)
  res = lapply(type, function(x) {
    xml_find_all(nodes, paste0("./", x))
  })
  names(res) = type
  L = sapply(res, length)
  res = res[L > 0]
  res = lapply(res, xml2::as_list)

  return(res)

}