Test Cifti updated Package
================
myc
2/7/2019

# Load MSC data and test data from cifti package

`download_cifti_data()`

## For reference: MSC 01 (65890 \* 818) (using matlab ft\_read\_cifti), showing first 5 rows & columns

    ##            V1       V2       V3       V4       V5
    ## [1,]  1.60060  2.27580  1.60540  0.12593 -0.49364
    ## [2,]  1.85790  2.55780  1.59500 -0.84614 -2.63840
    ## [3,]  0.33651  1.46200  1.77790  0.34857 -1.93580
    ## [4,]  0.95052  0.71743 -0.94483 -2.33310 -2.10040
    ## [5,] -2.08590 -1.99750 -1.24970 -0.93712 -0.58043

## MSC Sub-01 Session 1 (sub-MSC01\_ses-func01\_task-rest\_bold\_32k\_fsLR.dtseries.nii)

`read_cifti(fname = , trans_data = FALSE)` \* header dimension shows
65890 \* 818 \* read in file shows 65890 \* 818 by setting trans\_data
to *FALSE*

``` r
msc$hdr
```

    ## NIfTI-1 format
    ##   Type            : nifti
    ##   Data Type       : 16 (FLOAT32)
    ##   Bits per Pixel  : 32
    ##   Slice Code      : 0 (Unknown)
    ##   Intent Code     : 3002 ()
    ##   Qform Code      : 0 (Unknown)
    ##   Sform Code      : 0 (Unknown)
    ##   Dimension       : 1 x 1 x 1 x 1 x 65890 x 818
    ##   Pixel Dimension : 1 x 1 x 1 x 1 x 1 x 1
    ##   Voxel Units     :
    ##   Time Units      : sec

``` r
dim(msc$data)
```

    ## [1] 65890   818

``` r
msc$data[1:5,1:5]
```

    ##           [,1]      [,2]      [,3]      [,4]      [,5]
    ## [1,]  1.600599  2.275847  1.605402  0.125929 -0.493640
    ## [2,]  1.857941  2.557801  1.595005 -0.846137 -2.638434
    ## [3,]  0.336508  1.461959  1.777888  0.348566 -1.935833
    ## [4,]  0.950522  0.717431 -0.944832 -2.333094 -2.100426
    ## [5,] -2.085948 -1.997536 -1.249652 -0.937115 -0.580428

  - If transepose is not set to *FALSE* (default is *TRUE*), it will
    return a 818 \* 65890 that is filled correctly, and user just have
    to `t()` the `cii$data`

<!-- end list -->

``` r
t(msc_trans$data[1:5,1:5])
```

    ##           [,1]      [,2]      [,3]      [,4]      [,5]
    ## [1,]  1.600599  2.275847  1.605402  0.125929 -0.493640
    ## [2,]  1.857941  2.557801  1.595005 -0.846137 -2.638434
    ## [3,]  0.336508  1.461959  1.777888  0.348566 -1.935833
    ## [4,]  0.950522  0.717431 -0.944832 -2.333094 -2.100426
    ## [5,] -2.085948 -1.997536 -1.249652 -0.937115 -0.580428

## Data where dimensions are row x dense loads fine. Updated function loads them the same way as before.

### Conte69.MyelinAndCorrThickness.32k\_fs\_LR.dscalar.nii

  - Dense file “d”, vertex level data
  - Header shows dimension as rows \* dense

<!-- end list -->

``` r
ds$hdr
```

    ## NIfTI-1 format
    ##   Type            : nifti
    ##   Data Type       : 16 (FLOAT32)
    ##   Bits per Pixel  : 32
    ##   Slice Code      : 0 (Unknown)
    ##   Intent Code     : 3006 ()
    ##   Qform Code      : 0 (Unknown)
    ##   Sform Code      : 0 (Unknown)
    ##   Dimension       : 1 x 1 x 1 x 1 x 2 x 60951
    ##   Pixel Dimension : 1 x 1 x 1 x 1 x 1 x 1
    ##   Voxel Units     :
    ##   Time Units      : sec

``` r
attributes(ds$BrainModel[[1]])
```

    ## $IndexOffset
    ## [1] 0
    ## 
    ## $IndexCount
    ## [1] 30424
    ## 
    ## $BrainStructure
    ## [1] "CIFTI_STRUCTURE_CORTEX_LEFT"
    ## 
    ## $ModelType
    ## [1] "CIFTI_MODEL_TYPE_SURFACE"
    ## 
    ## $SurfaceNumberOfVertices
    ## [1] 32492

``` r
attributes(ds$BrainModel[[2]])
```

    ## $IndexOffset
    ## [1] 30424
    ## 
    ## $IndexCount
    ## [1] 30527
    ## 
    ## $BrainStructure
    ## [1] "CIFTI_STRUCTURE_CORTEX_RIGHT"
    ## 
    ## $ModelType
    ## [1] "CIFTI_MODEL_TYPE_SURFACE"
    ## 
    ## $SurfaceNumberOfVertices
    ## [1] 32492

``` r
dim(ds$data)
```

    ## [1] 60951     2

``` r
head(ds$data)
```

    ##          [,1]     [,2]
    ## [1,] 1.321855 3.195882
    ## [2,] 1.373803 2.141433
    ## [3,] 1.408264 3.028383
    ## [4,] 1.244012 3.237031
    ## [5,] 1.479665 2.216135
    ## [6,] 1.266741 2.795304

### Conte69.MyelinAndCorrThickness.32k\_fs\_LR.dtseries.nii

``` r
dt$hdr
```

    ## NIfTI-1 format
    ##   Type            : nifti
    ##   Data Type       : 16 (FLOAT32)
    ##   Bits per Pixel  : 32
    ##   Slice Code      : 0 (Unknown)
    ##   Intent Code     : 3002 ()
    ##   Qform Code      : 0 (Unknown)
    ##   Sform Code      : 0 (Unknown)
    ##   Dimension       : 1 x 1 x 1 x 1 x 2 x 60951
    ##   Pixel Dimension : 1 x 1 x 1 x 1 x 1 x 1
    ##   Voxel Units     :
    ##   Time Units      : sec

``` r
attributes(dt$BrainModel[[1]])
```

    ## $IndexOffset
    ## [1] 0
    ## 
    ## $IndexCount
    ## [1] 30424
    ## 
    ## $BrainStructure
    ## [1] "CIFTI_STRUCTURE_CORTEX_LEFT"
    ## 
    ## $ModelType
    ## [1] "CIFTI_MODEL_TYPE_SURFACE"
    ## 
    ## $SurfaceNumberOfVertices
    ## [1] 32492

``` r
attributes(dt$BrainModel[[2]])
```

    ## $IndexOffset
    ## [1] 30424
    ## 
    ## $IndexCount
    ## [1] 30527
    ## 
    ## $BrainStructure
    ## [1] "CIFTI_STRUCTURE_CORTEX_RIGHT"
    ## 
    ## $ModelType
    ## [1] "CIFTI_MODEL_TYPE_SURFACE"
    ## 
    ## $SurfaceNumberOfVertices
    ## [1] 32492

``` r
dim(dt$data)
```

    ## [1] 60951     2

``` r
head(dt$data)
```

    ##          [,1]     [,2]
    ## [1,] 1.321855 3.195882
    ## [2,] 1.373803 2.141433
    ## [3,] 1.408264 3.028383
    ## [4,] 1.244012 3.237031
    ## [5,] 1.479665 2.216135
    ## [6,] 1.266741 2.795304

### Conte69.MyelinAndCorrThickness.32k\_fs\_LR.ptseries.nii

  - parcel level

<!-- end list -->

``` r
pt$hdr
```

    ## NIfTI-1 format
    ##   Type            : nifti
    ##   Data Type       : 16 (FLOAT32)
    ##   Bits per Pixel  : 32
    ##   Slice Code      : 0 (Unknown)
    ##   Intent Code     : 3004 ()
    ##   Qform Code      : 0 (Unknown)
    ##   Sform Code      : 0 (Unknown)
    ##   Dimension       : 1 x 1 x 1 x 1 x 2 x 54
    ##   Pixel Dimension : 1 x 1 x 1 x 1 x 1 x 1
    ##   Voxel Units     :
    ##   Time Units      : sec

``` r
dim(pt$data)
```

    ## [1] 54  2

``` r
head(pt$data)
```

    ##          [,1]     [,2]
    ## [1,] 1.431012 2.515503
    ## [2,] 1.392534 2.339441
    ## [3,] 1.521327 2.154038
    ## [4,] 1.498012 1.684434
    ## [5,] 1.618780 2.652969
    ## [6,] 1.511189 2.089453

### Conte69.parcellations\_VGD11b.32k\_fs\_LR.dlabel.nii (dense label on vertex level)

  - dense label

<!-- end list -->

``` r
dl$hdr
```

    ## NIfTI-1 format
    ##   Type            : nifti
    ##   Data Type       : 16 (FLOAT32)
    ##   Bits per Pixel  : 32
    ##   Slice Code      : 0 (Unknown)
    ##   Intent Code     : 3007 ()
    ##   Qform Code      : 0 (Unknown)
    ##   Sform Code      : 0 (Unknown)
    ##   Dimension       : 1 x 1 x 1 x 1 x 3 x 64984
    ##   Pixel Dimension : 1 x 1 x 1 x 1 x 1 x 1
    ##   Voxel Units     :
    ##   Time Units      : sec

``` r
dim(dl$data)
```

    ## [1] 64984     3

``` r
head(dl$data)
```

    ##      [,1] [,2] [,3]
    ## [1,]    0   67    0
    ## [2,]    0   59    0
    ## [3,]    8   56    0
    ## [4,]    8   54    0
    ## [5,]   14   87    0
    ## [6,]    0   64    0
