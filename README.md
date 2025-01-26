# Protocol for spatial metabolomics and isotope tracing in the mouse brain

This repository includes the code, raw data, and analysis results for cluster and enrichment analyses of brain region datasets. The project involves comparing clusters across different brain regions (e.g., cortex, cerebellum) using mass spectrometry imaging (MSI) data and performing isotopologue enrichment analysis.

## Project Structure

- **Code**: includes all R scripts and R Markdown files used for data processing and analysis.
- **Input**: contains the raw data files required for analysis.
- **Results**: holds the processed data and results generated from the analysis.
- **Docs**: contains the HTML documents generated from R Markdown, which provide detailed analysis and visualizations.

---

## Documents

You can view the analysis for each dataset by clicking on the links below:

### Cluster analysis (Figure 4C)

1. [Brain Region Cluster Analysis](https://github.com/Neurometabolomics/Protocol_Spatial_Neurometabolomics/blob/main/Code/Brain_Region_Cluster_Analysis.Rmd)
2. [Cortex Region Cluster Analysis](https://github.com/Neurometabolomics/Protocol_Spatial_Neurometabolomics/blob/main/Code/Cortex_Region_Cluster_Analysis.Rmd)
3. [Cerebellum Region Cluster Analysis](https://github.com/Neurometabolomics/Protocol_Spatial_Neurometabolomics/blob/main/Code/Cereb_Region_Cluster_Analysis.Rmd)

### Enrichment analysis (Figure 4E and 4F)

4. [Enrichment Analysis](https://github.com/Neurometabolomics/Protocol_Spatial_Neurometabolomics/blob/main/Code/Enrichment_Analysis.Rmd)

---

## Input data

All raw data files used for this analysis are stored in the **Input** folder. Each dataset is organized into subfolders, such as `Input/4C_brain`, for better clarity and accessibility.

---

## Results

The processed results, including summary statistics, corrected isotopologue data, and visualizations, are stored in the **Results** folder. These results are categorized based on the dataset analyzed, such as `Results/4C_brain` for cluster analysis or `Results/4EF` for enrichment analysis.

---

## Usage

To replicate this analysis:

1. Clone the repository:
   ```bash
   git clone https://github.com/Numliap/STAR_Protocol.git
