# STAR Protocol: Cluster Analysis for Brain Region Data (Figure 4C)

This repository contains the code, raw data, and analysis results for the cluster analysis of brain region datasets. The project involves comparing clusters across different brain regions such as cortex and cerebellum, using mass spectrometry imaging (MSI) data.

## Project Structure

- **Code**: Contains all R scripts and R Markdown files used for data processing and analysis.
- **Input**: Contains the raw data files required for analysis.
- **Results**: Contains the processed data and results generated from the analysis.
- **Docs**: Contains the HTML documents generated from R Markdown, which provide detailed analysis and visualizations.

## Documents

You can view the analysis for each dataset by clicking on the links below:

1. [Brain Region Cluster Analysis](Docs/Brain_Region_Cluster_Analysis.html)
2. [Cortex Region Cluster Analysis](Docs/Cortex_Region_Cluster_Analysis.html)
3. [Cerebellum Region Cluster Analysis](Docs/Cerebellum_Region_Cluster_Analysis.html)

## Input Data

All raw data files used for this analysis are stored in the **Input** folder. Each dataset is organized into subfolders, such as `Input/4C_brain`, for better clarity and accessibility.

## Results

The processed results, including summary statistics and visualizations, are stored in the **Results** folder. These results are categorized based on the dataset analyzed, such as `Results/4C_brain`.

## Usage

To replicate this analysis:
1. Clone the repository: `git clone https://github.com/your-username/your-repo.git`
2. Open the RStudio project file (`.Rproj`) in the root directory.
3. Follow the instructions in the relevant R Markdown file located in the **Code** folder.
4. Generate the corresponding HTML documents in the **Docs** folder.

## Notes

- The comparison excludes the **Polyketides** superclass because it was only observed in the `Brain` dataset with minimal intensity (~0.01%) and was not present in `Cortex` or `Cerebellum`. This ensures consistent comparisons between datasets.
- Ensure that the working directory is set appropriately when running the R Markdown files.

## Questions or Issues

If you have any questions or issues, please open an issue in this repository.

---

**Author**: Watit Sontising  
**Last Updated**: January 17, 2025