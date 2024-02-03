The code in this repository reproduces the analysis presented in ["How well does the implementation of corporate zero-deforestation commitments in Indonesia align with aims to halt deforestation and include smallholders?"](publicationlink).

## Authors
The study was designed by [Adelina Chandra, Rachael Garrett, Kimberly Carlson, Robert Heilmayr, Matthieu Stigler, Jason Benedict, Janina Grabs]. The code was written by [Adelina Chandra].

## Data Download

Pre-processed data used in this analysis can be downloaded from [Zenodo](link). Other data is available upon request.

## Description

The code in this repository performs the following analysis steps:

1. **Company-level ZDC Quality Assessment**: Reproducing ZDC scores for 2018, 2019, and 2020 of palm oil companies with a sourcing base in Indonesia. The output includes company-level ZDC scores, both policy scores and aggregated scores of policy and implementation.
   - Company-level score assessment: `1_1_company_level_zdc_quality_assessment.R`.

3. **Mill-level ZDC Quality Assignment**: Assigning ZDC quality score for 2018, 2019, and 2020 as well as ZDC score via smallholder inclusion for 2020 to mills in operation in Indonesia in 2020. The analysis includes the following scripts:
   - Preparing mill-level ZDC quality score matching and aggregation: `2_1_mill_level_score_aggregation_preparation.R`.
   - Mill-level ZDC quality score assignment: `2_2_mill_level_zdc_quality_assignment.R`.
   - Mill-level ZDC quality score via smallholder inclusion assignment: `2_3_mill_level_zdc_quality_via_smallholder_inclusion_assignment.R`.

4. **Grid-level Results Visualization**: Reproducing visualization (figures and maps) with provided grid-level data. This step is covered in the following scripts:
   - Grid-level results visualization: `3_1_grid_level_result_visualization.R`.
   - Grid-level Spatial and Functional fit: `3_2_grid_level_spatial_and_functional_fit_mapping.R`.

Please note that the scripts should be executed in the specified sequence for the proper flow of the analysis.

If you have any questions or encounter issues, please feel free to [contact us](mailto:adelinachandra@gmail.com).
