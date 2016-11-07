# rm(list = ls())
library(xml2)
library(gifti)
library(rgl)
library(RColorBrewer)
library(matrixStats)
onefile=TRUE
gzipped=FALSE
verbose=TRUE
warn=-1
reorient=FALSE
call=NULL
fname = "~/Downloads/cifti-2_test_data/Conte69.MyelinAndCorrThickness.32k_fs_LR.dscalar.nii"
# fname = "~/Downloads/cifti-2_test_data/hcp_atlas_clean.dtseries.nii"
surf_fname = "~/Downloads/cifti-2_test_data/Conte69.L.inflated.32k_fs_LR.surf.gii"
surf = readgii(surf_fname)






  make_attr = function(x, which, value) {
      mapply(function(w, v) {
        attr(x, which = w) = v
      }, which, value, SIMPLIFY = FALSE)
      return(x)
  }





  # xpath = switch(cifti_version,
  #   # "1.0" = '/CIFTI/Matrix/Volume',
  #   # "1" = '/CIFTI/Matrix/Volume',
  #   "2.0" = './BrainModel',
  #   "2" = './BrainModel'
  #   )




nim = nifti_2_hdr(fname)

xmldata = cifti_xml(fname)
data = cifti_data(fname, nim = nim)
data = drop(data)
data = t(data)
doc = read_xml(xmldata)
doc = xml_ns_strip(doc)
# xml_attrs(doc)
# L = list(nim)
# return(nim)
doc_attrs = xml_attrs(doc)
cifti_version = doc_attrs["Version"]
stopifnot(cifti_version %in% c("2.0", "2"))

nodes = xml_find_all(doc,
                     "/CIFTI/Matrix/MatrixIndicesMap")

run_attr = xml_attrs(nodes)
types = c("Volume", "Surface",
          "Parcel",  "NamedMap",
          "BrainModel")


  nodeset = xml_find_all(nodes, "./Volume")
  vols = parse_volume(nodeset)

  nodeset = xml_find_all(nodes, "./BrainModel")

  verts = parse_brain_model(nodeset)

 nodeset = xml_find_all(nodes, "./NamedMap")

lt = parse_named_map(nodeset)

nodeset = xml_find_all(nodes, "./Surface")
surfs = parse_surface(nodeset)
nodeset = xml_find_all(nodes, "./Parcel")
pars = parse_parcel(nodeset)

xL = L = surf$data

reg_verts =  L$vertices
faces = as.vector(t(L$faces) + 1)
surf_verts = L$vertices[faces,]
# norms = L$normals[faces,]

ind = verts[[1]] + 1
# faces = L$faces
# run_verts = run_verts[faces, ]

good_data = rep(NA, length = nrow(reg_verts))
good_data[ind] = data[, 1]

cdata = good_data[faces]
# d = data[ind, 1]
# nfaces = length(faces)
# cdata = vector(length = nfaces)
# cdata[ faces %in% ind ] = d[ faces[ faces %in% ind ] ]
# dim(surf_verts)
# keep_ind = faces %in% ind


cols = brewer.pal(10, "Spectral")
# cols = brewer.pal(10, "Reds")
mypal = colorRampPalette(colors = cols)
# mypal = colorRampPalette(colors = c("red", "black"))
n = 20
# breaks = seq(min(cdata), max(cdata), length.out = n)
grid = 0.4
breaks = seq(min(cdata, na.rm = TRUE),
   max(cdata, na.rm = TRUE) + grid, by = grid)
ints = cut(cdata, include.lowest = TRUE,
  breaks = breaks)
ints = as.integer(ints)
# stopifnot(!any(is.na(ints)))
cols = mypal(n)[ints]
cols = scales::alpha(cols, 1)


# cols = scales::alpha(cols, 1)
rgl.open()
rgl.triangles(x = surf_verts, color = cols)
# rgl.triangles(x = surf_verts, coloo)
# rgl.triangles(x = nii_verts)


