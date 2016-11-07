i = 1
# for (i in 1:length(nodes)) {
node = nodes[[i]]
dtype = xml_attr(node, "IndicesMapToDataType")
which_dim = xml_attr(node, "AppliesToMatrixDimension")
which_dim = as.numeric(which_dim) + 1

dim_order = switch(
  dtype,
  'CIFTI_INDEX_TYPE_BRAIN_MODELS' = "pos",
  "CIFTI_INDEX_TYPE_PARCELS" = "chan",
  "CIFTI_INDEX_TYPE_SERIES" = "sometime",
  "CIFTI_INDEX_TYPE_SCALARS" = "",
  "CIFTI_INDEX_TYPE_LABELS" = "",
  "CIFTI_INDEX_TYPE_TIME_POINTS" = "time")
if (dtype == "CIFTI_INDEX_TYPE_SERIES") {
  time_unit = xml_attr(node, "SeriesUnit")
  dim_order = switch(
    time_unit,
    "SECOND" = "time",
    "HZ" = "freq")
}

if (dtype == "CIFTI_INDEX_TYPE_TIME_POINTS") {
  time_unit = xml_attr(node, "TimeStepUnits")
  if (time_unit %in% 'NIFTI_UNITS_SEC') {
    Cifti.fsample = 1/str2double(MatrixIndicesMap(i).TimeStep);
  } else {
    stop("Unsupported timeframe")
  }
}
#
# dimord(MatrixIndicesMap(i).AppliesToMatrixDimension+1) = {'pos'};
#
# % concatenate all into an array of brain models
# for j=1:length(MatrixIndicesMap(i).BrainModel)
# BrainModel = cat(1, BrainModel, MatrixIndicesMap(i).BrainModel(j));
# end
#
# case 'CIFTI_INDEX_TYPE_PARCELS'
# dimord(MatrixIndicesMap(i).AppliesToMatrixDimension+1) = {'chan'};
#
# % construct an array of brain models
# IndexOffset = 0;
# BrainModelParcelName = {};
#
# for j=1:numel(Parcel)
#
# for k=1:numel(Parcel(j).BrainStructure)
# sel = strcmp({Surface(:).BrainStructure}, Parcel(j).BrainStructure{k});
# BrainModel(end+1).IndexOffset             = IndexOffset;
# BrainModel(end  ).IndexCount              = numel(Parcel(j).Vertices{k});
# BrainModel(end  ).BrainStructure          = Parcel(j).BrainStructure{k};
# BrainModel(end  ).ModelType               = 'CIFTI_MODEL_TYPE_SURFACE';
# BrainModel(end  ).SurfaceNumberOfVertices = Surface(sel).SurfaceNumberOfVertices;
# BrainModel(end  ).VertexIndices           = Parcel(j).Vertices{k};
# BrainModel(end  ).VoxelIndicesIJK         = [];
# IndexOffset = IndexOffset + numel(Parcel(j).Vertices{k});
# BrainModelParcelName{end+1} = Parcel(j).Name;
# end % for each BrainStructure
#
# if ~isempty(Parcel(j).VoxelIndicesIJK)
# BrainModel(end+1).IndexOffset             = IndexOffset;
# BrainModel(end  ).IndexCount              = size(Parcel(j).VoxelIndicesIJK,1);
# BrainModel(end  ).BrainStructure          = 'CIFTI_STRUCTURE_INVALID';
# BrainModel(end  ).ModelType               = 'CIFTI_MODEL_TYPE_VOXELS';
# BrainModel(end  ).SurfaceNumberOfVertices = [];
# BrainModel(end  ).VertexIndices           = [];
# BrainModel(end  ).VoxelIndicesIJK         = Parcel(j).VoxelIndicesIJK;
# IndexOffset = IndexOffset + size(Parcel(j).VoxelIndicesIJK,1);
# BrainModelParcelName{end+1} = Parcel(j).Name;
# end
#
# end % for each Parcel
#
# case 'CIFTI_INDEX_TYPE_SERIES'
# % this only applies to cifti version 2
# switch MatrixIndicesMap(i).SeriesUnit
# case 'SECOND'
# dimord(MatrixIndicesMap(i).AppliesToMatrixDimension+1) = {'time'};
# Cifti.time = (((1:MatrixIndicesMap(i).NumberOfSeriesPoints)-1) * MatrixIndicesMap(i).SeriesStep + MatrixIndicesMap(i).SeriesStart) * 10^MatrixIndicesMap(i).SeriesExponent;
# case 'HZ'
# dimord(MatrixIndicesMap(i).AppliesToMatrixDimension+1) = {'freq'};
# Cifti.freq = (((1:MatrixIndicesMap(i).NumberOfSeriesPoints)-1) * MatrixIndicesMap(i).SeriesStep + MatrixIndicesMap(i).SeriesStart) * 10^MatrixIndicesMap(i).SeriesExponent;
# % case 'METER'
# % case 'RADIAN'
# otherwise
# error('unsupported SeriesUnit');
# end % switch SeriesUnit
#
# case 'CIFTI_INDEX_TYPE_SCALARS'
# dimord{MatrixIndicesMap(i).AppliesToMatrixDimension+1} = []; % scalars are not explicitly represented
# for j=1:length(NamedMap)
# Cifti.mapname{j} = fixname(NamedMap(j).MapName);
# end
#
# case 'CIFTI_INDEX_TYPE_LABELS'
# dimord{MatrixIndicesMap(i).AppliesToMatrixDimension+1} = []; % labels are not explicitly represented
# for j=1:length(NamedMap)
# key = NamedMap(j).LabelTable.Key;
# lab = NamedMap(j).LabelTable.Label;
# sel = key>0;
# Cifti.labeltable{j}(key(sel)) = lab(sel);
# Cifti.mapname{j} = fixname(NamedMap(j).MapName);
# end
#
# case 'CIFTI_INDEX_TYPE_TIME_POINTS'
# % this only applies to cifti-1, in cifti-2 this has been replaced by CIFTI_INDEX_TYPE_SERIES
# dimord(MatrixIndicesMap(i).AppliesToMatrixDimension+1) = {'time'};
# switch MatrixIndicesMap(i).TimeStepUnits
# case 'NIFTI_UNITS_SEC'
# Cifti.fsample = 1/str2double(MatrixIndicesMap(i).TimeStep);
# otherwise
# % other units should be trivial to implement
# error('unsupported TimeStepUnits "%s"', MatrixIndicesMap(i).TimeStepUnits);
# end % switch TimeStepUnits
#
# otherwise
# error('unsupported IndicesMapToDataType');
# end % switch IndicesMapToDataType
# end % for each MatrixIndicesMap
