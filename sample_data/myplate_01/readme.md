# Overall description
Effect of siRNA treatments on the traffic of VSVG, a secretory protein.

## Dataset filename
`image_concat.csv`

## Assay information
- 96 Well Plate
- 1 imaging field per well
- 1 timepoint (fixed cells)
- The plate contains:
  - 81 siRNAs targeting 81 genes (1 siRNA per well)
  - 1 well was treated with "Scrambled" siRNA (_i.e._ Negative control)
  - 3 wells were not treated with siRNA (_i.e._ "empty")
- File and folder names indicate the treatment in each well
- Original files have been downscaled and compressed (jpg)
- Simulated data, no real biological meaning
- 3 Fluorescence channels:

| Filename     | Meaning                                                   |
| ------------ |---------------------------------------------------------- |
| `*DNA.jpg`   | Nuclei                                                    |
| `*TOTAL.jpg` | VSVG-GFP (_i.e._ total cellular VSVG)                     |
| `*PM.jpg`    | Staining of VSVG molecules located at the plasma membrane |


## Settings for analyzing this dataset in shinyHTM 
- **Treatment column**: `Treatment`
- **Batch column**: `Metadata_platePath`
- **Well coordinate column**: `Metadata_wellNum`
- **Sub-position column:** `Metadata_posNum`
- **Image root folder name in table:** `c:\tutorial\myplate_01`
- **Prefix: column with folder name:** `PathName_`
- **Prefix: column with file name:** `FileName_`
- **Number of Rows:** `8`
- **Number of Columns:** `12`
- **Number of subposition Rows:** `1`
- **Number of subposition Columns:** `1`
- **Negative control**: `Scrambled`
- **Positive control**: `None`
- **Number of objects per image:** `Count_cell_final`


## Meaning of the columns in the *.csv file

This csv file was generated with CellProfiler.  
Each line in the file corresponds to 1 imaging field and contains all the "measurements" made in that field.

|    | Column name                                                    | Meaning |
|--- | -------------------------------------------------------------- |-------------- |
| 1  | `Count_cell_all`                                               | Number of all cells in the image. |
| 2  | `Count_cell_final`                                             | A subset of Count_cell_all which excludes cells with saturated fluorescence, aberrant phenotypes and cells touching the image edge. |
| 3  | `ImageNumber`                                                  | Sequential number of the imaging field. |
| 4  | `ImageQuality_PowerLogLogSlope_nucleus`                        | A metric for blur detection. [more info](http://d1zymp9ayga15t.cloudfront.net/CPmanual/MeasureImageQuality.html) |
| 5  | `Intensity_MedianIntensity_PM_bgCorr_bgMask`                   | Median fluorescence intensity in cell-free regions (PM channel). Reports on background flurorescence. |
| 6  | `Intensity_MedianIntensity_TOTAL_bgCorr_bgMask`                | Median fluorescence intensity in cell-free regions (Total channel). Reports on background flurorescence. |
| 7  | `Mean_cell_final_Intensity_IntegratedIntensity_PM_bgCorr`      | The mean of the integrated fluorescence intensity in the "PM" channel, calculated across all cells in an image. |
| 8  | `Mean_cell_final_Intensity_IntegratedIntensity_TOTAL_bgCorr`   | The mean of the integrated fluorescence intensity in the "TOTAL" channel, calculated across all cells in an image. |
| 9  | `Mean_cell_final_Math_ratio`                                   | The mean of the integrated fluorescence intensity in the "PM" channel divided by the integrated fluorescence intensity in the "TOTAL" channel, calculated across all cells in an image. **Reports on traffic efficiency.** |
| 10 | `Mean_nucleus_final_AreaShape_Area`                            | The mean of area of all cell nuclei in the field, in pixel<sup>2</sup> units. |
| 11 | `Mean_nucleus_final_AreaShape_Center_X`                        | The mean x-coordinate of the "[center](http://d1zymp9ayga15t.cloudfront.net/CPmanual/MeasureObjectSizeShape.html)" of all cell nuclei in the field, in pixel units. |
| 12 | `Mean_nucleus_final_AreaShape_Center_Y`                        | The mean y-coordinate of the "[center](http://d1zymp9ayga15t.cloudfront.net/CPmanual/MeasureObjectSizeShape.html)" of all cell nuclei in the field, in pixel units. |
| 13 | `Mean_nucleus_final_AreaShape_Compactness`                     | The variance of the radial distance of a nucleus' pixels from the centroid divided by the area. Mean value calculated across all nuclei in an image. |
| 14 | `Mean_nucleus_final_AreaShape_Eccentricity`                    | The ratio of the distance between the foci of the ellipse that has the same second-moments as a nucleus and its major axis length. Mean value calculated across all nuclei in an image. |
| 15 | `Mean_nucleus_final_AreaShape_MaxFeretDiameter`                | The largest possible diameter which can be fitted inside a cell nucleus, in pixel units. Mean value calculated across all nuclei in an image. |
| 16 | `Mean_nucleus_final_AreaShape_MaximumRadius`                   | The maximum distance of any pixel in a nucleus to the closest pixel outside of the nucleus. Mean value calculated across all nuclei in an image. |
| 17 | `Mean_nucleus_final_AreaShape_MeanRadius`                      | The mean distance of any pixel in the nucleus to the closest pixel outside of the nucleus. Mean value calculated across all nuclei in an image. |
| 18 | `Mean_nucleus_final_AreaShape_MedianRadius`                    | The median distance of any pixel in the cell nuclei to the closest pixel outside of the nuclei. Mean value across all cell nuclei in the image. |
| 19 | `Mean_nucleus_final_AreaShape_MinorAxisLength`                 | The length (in pixels) of the minor axis of the ellipse that has the same normalized second central moments as the region. Mean value across all cell nuclei in the image. |
| 20 | `Mean_nucleus_final_AreaShape_Orientation`                     | The angle (in degrees ranging from -90 to 90 degrees) between the x-axis and the major axis of the ellipse that has the same second-moments as the region. Mean value across all cell nuclei in the image. |
| 21 | `Mean_nucleus_final_AreaShape_Perimeter`                       | The mean perimeter of all cell nuclei in the field, in pixel units. |
| 22 | `Mean_nucleus_final_AreaShape_Solidity`                        | The proportion of the pixels in the convex hull that are also in the object. [more info](http://d1zymp9ayga15t.cloudfront.net/CPmanual/MeasureObjectSizeShape.html). Mean value across all cell nuclei in the image. |
| 23 | `Median_cell_final_Intensity_IntegratedIntensity_PM_bgCorr`    | The mean of the integrated fluorescence intensity in the "PM" channel, calculated across all cells in an image. |
| 24 | `Median_cell_final_Intensity_IntegratedIntensity_TOTAL_bgCorr` | The mean of the integrated fluorescence intensity in the "TOTAL" channel, calculated across all cells in an image. |
| 25 | `Median_cell_final_Math_ratio`                                 | The mean of the integrated fluorescence intensity in the "PM" channel divided by the integrated fluorescence intensity in the "TOTAL" channel, calculated across all cells in an image. **Reports on traffic efficiency.** |
| 26 | `Metadata_gene`                                                | The gene target of the siRNA used in the well. Official gene symbol. |
| 27 | `Metadata_imageBaseName`                                       | The full image filename, excluding the channel suffix and file extension. |
| 28 | `Metadata_pathBase`                                            | The mother folder where all files were located during the CellProfiler analysis. In case, a Windows PC. |
| 29 | `Metadata_plateName`                                           | The plate name excluding the experiment replicate serial number. |
| 30 | `Metadata_platePath`                                           | The subfolder containing all images for a plate. It is also the full name of the plate. |
| 31 | `Metadata_plateRepl`                                           | The serial number of the experiment replicate. This dataset only includes 1 plate. |
| 32 | `Metadata_posNum`                                              | The number of the image field within a well. This dataset only has 1 imaging field per well. |
| 33 | `Metadata_posPath`                                             | The name of the subfolder with all images regarding a specific imaging field. |
| 34 | `Metadata_siRNA`                                               | The catalog number of the siRNA used in the well. |
| 35 | `Metadata_wellNum`                                             | Well number. |
| 36 | `Metadata_wellPath`                                            | The name of the folder containing all images acquired for a given well. |
| 37 | `FileName_PM`                                                  | The file name of the image of the "PM" channel. |
| 38 | `FileName_TOTAL`                                               | The file name of the image of the "TOTAL" channel. |
| 39 | `FileName_nucleus`                                             | The file name of the image of the "DNA" channel. |
| 40 | `PathName_PM`                                                  | The full folder path of the image of the "PM" channel. |
| 41 | `PathName_TOTAL`                                               | The full folder path of the image of the "TOTAL" channel. |
| 42 | `PathName_nucleus`                                             | The full folder path of the image of the "DNA" channel. |
| 43 | `Treatment`                                                    | The well treatment. A concatenated text string with the structure "gene_siRNA". |





### References

- Botelho HM _et al_ (2015) **Protein traffic disorders: an effective high-throughput fluorescence microscopy pipeline for drug discovery**  _Sci Rep_ 5:9038 [[PubMed](http://www.ncbi.nlm.nih.gov/pubmed/25762484)]

- Almaça J _et al_ (2011) **Functional genomics assays to study CFTR traffic and ENaC function** _Methods Mol Biol_ 742:249-64 [[PubMed](http://www.ncbi.nlm.nih.gov/pubmed/21547737)]

- Carpenter AE _et al_ (2006) **CellProfiler: image analysis software for identifying and quantifying cell phenotypes** _Genome Biology_ 7:R100 [[PubMed]](http://www.ncbi.nlm.nih.gov/pubmed/17076895)