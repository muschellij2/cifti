
#' @title Test of numeric CIFTI field
#' @description Wrapper of CIFTI fields for easy logical test
#' @param x character vector of names
#'
#' @return Logical of length same as x
#' @export
is_cifti_numeric = function(x) {
  num_attrs = c('NumberOfMatrices',
                'AppliesToMatrixDimension',
                'IndexOffset', 'IndexCount',
                'SurfaceNumberOfNodes', 'VolumeDimensions',
                'SurfaceNumberOfVertices', 'SeriesStart',
                'SeriesStep', 'NumberOfSeriesPoints',
                'SeriesExponent', 'Vertices', 'MeterExponent')
  x %in% num_attrs
}