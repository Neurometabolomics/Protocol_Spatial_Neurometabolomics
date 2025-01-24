# Protocol for spatial metabolomics and isotope tracing in the mouse brain

This repository contains the code, raw data, and analysis results for cluster and enrichment analyses of brain region datasets. The project involves comparing clusters across different brain regions (e.g., Cortex, Cerebellum) using mass spectrometry imaging (MSI) data and performing isotopologue enrichment analysis.

## Project Structure

- **Code**: Contains all R scripts and R Markdown files used for data processing and analysis.
- **Input**: Contains the raw data files required for analysis.
- **Results**: Contains the processed data and results generated from the analysis.
- **Docs**: Contains the HTML documents generated from R Markdown, which provide detailed analysis and visualizations.

---

## Documents

You can view the analysis for each dataset by clicking on the links below:

### Cluster Analysis (Figure 4C)

1. [Brain Region Cluster Analysis](https://numliap.github.io/STAR_Protocol/Brain_Region_Cluster_Analysis.html)
2. [Cortex Region Cluster Analysis](https://numliap.github.io/STAR_Protocol/Cortex_Region_Cluster_Analysis.html)
3. [Cerebellum Region Cluster Analysis](https://numliap.github.io/STAR_Protocol/Cereb_Region_Cluster_Analysis.html)

### Enrichment Analysis (Figure 4E and 4F)

4. [Enrichment Analysis](https://numliap.github.io/STAR_Protocol/Enrichment_Analysis.html)

---

## Input Data

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
