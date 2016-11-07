#' @title Get Generic CIFTI Type
#' @description Wrapper for multiple types of CIFTI XML types.
#' @param fname File name of CIFTI file
#' @param type type of data to extract from CIFTI XML
#'
#' @return List of output from each type
#' @export
get_cifti_type = function(
  fname,
  type = c("Volume", "Surface",
           "Parcel",  "NamedMap",
           "BrainModel")) {
  type = match.arg(type, several.ok = TRUE)
  func_name = function(run_type) {
    run_type = tolower(run_type)
    run_type[ run_type %in% "brainmodel" ] = "brain_model"
    run_type[ run_type %in% "namedmap" ] = "named_map"
    paste0("get_", run_type)
  }
  funcs = func_name(type)

  args = list(fname = fname)
  res = lapply(funcs, do.call, args = args)
  names(res) = type
  L = sapply(res, length)
  res = res[L > 0]
  return(res)

}