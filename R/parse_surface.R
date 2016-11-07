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
