# Macrophage-built regression niches reveal humoral immunity after immunotherapy in advanced clear cell renal cell carcinoma

## Introduction

In this repository, you could access to the code used to analysis the Stereo-seq, Stereo-CITE-seq data from the associated manuscript.

Analysis: scripts used for the analysis of Stereo-seq and Stereo-CITE-seq data.
1) [Pathological_regions](https://github.com/Renji-RCC/RCC/tree/main/Analysis/Pathological_regions): scripts for mapping pathological regions to spatial coordinates and calculating their areas.
2) [TLS_shells](https://github.com/Renji-RCC/RCC/tree/main/Analysis/TLS_shells): scripts for generating 100-μm concentric shells extending from TLS centers and summarizing cell-type proportions and signature scores within each shell.
3) [boundary_expansion](https://github.com/Renji-RCC/RCC/tree/main/Analysis/boundary_expansion): scripts for generating boundary-centered expansion zones and calculating spatial distances from spatial programs to pathological-region boundaries.
4) [Spatial_clustering](https://github.com/Renji-RCC/RCC/tree/main/Analysis/Spatial_clustering): scripts for preparing spatial expression matrices, performing STAGATE and SpatialGlue clustering, and calculating Jaccard similarities between STAGATE-derived spatial domains.

Plotting: scripts used to generate figures in the manuscript.

## Data access
scRNA-seq and Stereo-seq data generated in this study have been deposited in the CNGB Sequence Archive ([CNSA]https://db.cngb.org/cnsa/) of the China National GeneBank DataBase (CNGBdb) under accession code CNP0004598. Processed data of Stereo-seq and Stereo-CITE have also been deposited in the Spatial Transcript Omics DataBase (STOmicsDB) of CNGBdb under accession code STT0000175.

## Contact
If you have questions about the data, please create a [new Issue](https://github.com/Renji-RCC/RCC/issues/new)