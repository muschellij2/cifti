nii_fname = function(fname) {
  gz = grepl("[.]gz$", tolower(fname))
  if (any(gz)) {
    fname[gz] = sapply(
      fname[gz],
      R.utils::gunzip,
      temporary = TRUE,
      overwrite = TRUE,
      remove = FALSE)
  }
  return(fname)
}

keep_sub_nodeset = function(nodeset) {
  n = sapply(nodeset, length)
  if (any(n > 0)) {
    nodeset = nodeset[n > 0]
  }
  if (length(nodeset) == 1) {
    nodeset = nodeset[[1]]
  }
  nodeset
}
