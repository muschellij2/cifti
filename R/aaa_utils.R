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
