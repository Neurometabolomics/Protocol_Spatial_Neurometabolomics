---
title: "Cluster analysis for brain region"
output: html_document
date: "2024-11-25"
editor_options: 
  chunk_output_type: console
---

This is document to create cluster image on Figure 4C_brain.

To begin our analysis, we first need to make sure that all the required R packages are installed and ready to use. In this section, we use a custom function to check if each package is already installed, and if not, it will install it for you automatically. This step ensures that you have all the tools you need to smoothly run the rest of the code. It’s a simple but crucial setup to avoid any interruptions later on.



To ensure that all input and output files are correctly accessed and saved, we specify the working directory where our data is stored. For this analysis, the directory is set to Input/4C_brain. However, your file path might be different, so make sure to set the working directory to the appropriate folder where your input files are stored. This folder should contain the data required for clustering and classification.


``` r
# Set up the working directory
# Update this path to match the location of your input files
setwd("~/Documents/GitHub/STAR_Protocol/Input/4C_brain")

# Print the current working directory to confirm
getwd()
```

```
## [1] "/Users/numliapmacprom2/Documents/GitHub/STAR_Protocol/Input/4C_brain"
```

Now that the working directory is set, the next step is to load the required data files. For this analysis, we will read the cluster data and the data matrix. The cluster files follow a sequential naming format (e.g., ClusterNo.1.csv, ClusterNo.2.csv, etc.), while the data matrix file is named Data_Matrix.csv. Ensure these files are present in your working directory. Both file types are generated from IMAGEREVEAL, as described in manuscript part 6, step 10.


``` r
# Function to read cluster files and combine them into a single dataframe
readCluster <- function(cluster_name, number_of_cluster) {
  df <- NULL
  for (i in 1:number_of_cluster) {
    file <- paste0(cluster_name, i, ".csv")  # Construct file name
    if (i == 1) {
      df <- read.csv(file)  # Read the first cluster file
      colnames(df)[colnames(df) == "Intensity"] <- paste0("Intensity", ".cluster", i)
    } else {
      tmp_df <- read.csv(file)  # Read subsequent cluster files
      colnames(tmp_df)[colnames(tmp_df) == "Intensity"] <- paste0("Intensity", ".cluster", i)
      df <- merge(df, tmp_df)  # Merge with the main dataframe
    }
  }
  return(df)
}

# Read the cluster files
cluster <- readCluster("ClusterNo.", 20)
```

```
## Warning in file(file, "rt"): cannot open file 'ClusterNo.1.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Round m/z values for consistency
cluster$m.z <- round(cluster$m.z, 4)

# Read the data matrix file
data_matrix <- read.csv("Data_Matrix.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'Data_Matrix.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Preview the data
head(cluster)
```

```
##        m.z Intensity.cluster1 Intensity.cluster2 Intensity.cluster3 Intensity.cluster4 Intensity.cluster5 Intensity.cluster6 Intensity.cluster7
## 1  76.0393          0.1442761          0.3929302          0.0299838          0.1837246          0.2185196          0.2355051          1.0348138
## 2  89.0597          0.2226788          0.1632750          0.2282751          0.1963871          0.0849793          0.1453071          0.0908355
## 3  89.1073          0.3082936          0.5927600          0.0000000          0.2868329          0.5273454          0.5735760          0.7420870
## 4  90.0550         83.5216220        105.4862700         63.4240346        108.4669570        125.7119275        280.5406887         83.9715056
## 5  98.0212          0.0000000          0.0000000          0.0000000          0.0000000          0.0000000          0.0000000          0.0000000
## 6 102.0550          0.2973465          0.3293756          0.3526377          0.3527773          0.2875921          0.1647493          0.2625769
##   Intensity.cluster8 Intensity.cluster9 Intensity.cluster10 Intensity.cluster11 Intensity.cluster12 Intensity.cluster13 Intensity.cluster14
## 1          0.1940557          0.6497177           0.0964330           0.1505571           0.1034768           0.2523033           0.1906133
## 2          0.1508301          0.3000713           0.1742996           0.1917549           0.2135133           0.3444325           0.1298350
## 3          0.4632995          0.7388196           0.3287836           0.4249475           0.0000000           0.3884697           0.3318152
## 4        129.1564853         69.4796682         145.1680149          85.0197596          43.6503745          95.0312664          92.6605302
## 5          0.0000000          0.0000000           0.0000000           0.0000000           0.0000000           0.0000000           0.0000000
## 6          0.3776691          0.1509223           0.2640540           0.2511292           0.4504674           0.3629882           0.3287221
##   Intensity.cluster15 Intensity.cluster16 Intensity.cluster17 Intensity.cluster18 Intensity.cluster19 Intensity.cluster20
## 1           1.3934011           0.3281161           0.5503210           0.3399630           0.2504313           0.7239582
## 2           0.1737150           0.0637115           0.1825224           0.1561711           0.0769163           0.1972481
## 3           0.5117637           0.6931002           0.6405906           0.5057812           0.5449819           0.7674770
## 4          55.1248950         212.1843165         177.5875341         165.1214300          77.9882114         100.8368378
## 5           0.0000000           0.0000000           0.0000000           0.0000000           0.0000000           0.0000000
## 6           0.1936912           0.1660350           0.2635150           0.1649331           0.3125222           0.3264301
```

``` r
head(data_matrix)
```

```
##   No.  Use Tag                                    Label      m.z Formula Adduct.Ion Matrix Polarity   All
## 1   1 TRUE  NA (1Z)-prop-1-ene-1,2,3-tricarboxylic acid 175.0237  C6H6O6        M+H   CHCA Positive 0.002
## 2   2 TRUE  NA (1Z)-prop-1-ene-1,2,3-tricarboxylic acid 197.0057  C6H6O6       M+Na   CHCA Positive 3.134
## 3   3 TRUE  NA (1Z)-prop-1-ene-1,2,3-tricarboxylic acid 212.9801  C6H6O6        M+K   CHCA Positive 0.001
## 4   4 TRUE  NA         (2-hydroxyethyl)trimethylazanium 105.1148 C5H14NO        M+H   CHCA Positive 0.004
## 5   5 TRUE  NA         (2-hydroxyethyl)trimethylazanium 127.0968 C5H14NO       M+Na   CHCA Positive 0.007
## 6   6 TRUE  NA         (2-hydroxyethyl)trimethylazanium 143.0712 C5H14NO        M+K   CHCA Positive 2.108
```

Next, we extract the relevant metadata (m/z values and labels) from the data matrix to prepare for classification using the RefMet library. This involves creating a new dataframe, tag, with rounded m/z values for consistency.


``` r
# Extract m/z values and labels from the data matrix
tag <- NULL
tag <- data.frame(
  m.z = round(data_matrix$m.z, 4),  # Round m/z values for consistency
  Label = data_matrix$Label        # Extract Shimadzu annotations
)

# Preview the data
head(tag)
```

```
##        m.z                                    Label
## 1 175.0237 (1Z)-prop-1-ene-1,2,3-tricarboxylic acid
## 2 197.0057 (1Z)-prop-1-ene-1,2,3-tricarboxylic acid
## 3 212.9801 (1Z)-prop-1-ene-1,2,3-tricarboxylic acid
## 4 105.1148         (2-hydroxyethyl)trimethylazanium
## 5 127.0968         (2-hydroxyethyl)trimethylazanium
## 6 143.0712         (2-hydroxyethyl)trimethylazanium
```

In this step, we use the RefMet database to classify metabolites based on their labels. This process involves sending the list of labels to the RefMet database via an API call and retrieving standardized metabolite classifications.

However, not all compounds will be classified due to mismatches between the Shimadzu library and RefMet or challenges in resolving isomers. To address this, we follow a structured approach:

1.  **Pre-classify the compounds**: Query the RefMet database for initial classifications.
2.  **Identify missing compounds**: Filter and inspect labels that could not be classified.
3.  **Manually add missing compounds**: Supplement the dataset with curated data from the `../metabolite_reference` directory.
4.  **Reclassify the dataset**: Use updated labels to refine classifications.
5.  **Fill remaining classification gaps manually**: Address any residual missing data directly.

### Step 1: Pre-classify the compounds

We begin by querying the RefMet database to retrieve initial classifications for all compounds.


``` r
# Pre-classify the compounds using the RefMet API
# Join all labels into a single string for RefMet API query
met_list <- stri_join_list(list(tag$Label), sep = "\n")

# Set up and execute the API call
h <- new_handle()
handle_setform(h, metabolite_name = met_list)
req <- curl_fetch_memory("https://www.metabolomicsworkbench.org/databases/refmet/name_to_refmet_new_minID.php", handle = h)

# Read and clean the RefMet output
refmet <- read.table(
  text = rawToChar(req$content),
  header = TRUE,
  na.strings = "-",
  stringsAsFactors = FALSE,
  quote = "",
  comment.char = "",
  sep = "\t"
)

# Replace missing values with a placeholder
refmet[is.na(refmet)] <- "-"
refmet[refmet == ""] <- "-"

# Preview the data
head(refmet)
```

```
##                                 Input.name Standardized.name Formula Exact.mass                Super.class Main.class Sub.class PubChem_CID ChEBI_ID
## 1 (1Z)-prop-1-ene-1,2,3-tricarboxylic acid cis-Aconitic acid  C6H6O6   174.0164              Organic acids  TCA acids TCA acids      643757    32805
## 2 (1Z)-prop-1-ene-1,2,3-tricarboxylic acid cis-Aconitic acid  C6H6O6   174.0164              Organic acids  TCA acids TCA acids      643757    32805
## 3 (1Z)-prop-1-ene-1,2,3-tricarboxylic acid cis-Aconitic acid  C6H6O6   174.0164              Organic acids  TCA acids TCA acids      643757    32805
## 4         (2-hydroxyethyl)trimethylazanium           Choline C5H14NO   104.1075 Organic nitrogen compounds   Cholines  Cholines         305    15354
## 5         (2-hydroxyethyl)trimethylazanium           Choline C5H14NO   104.1075 Organic nitrogen compounds   Cholines  Cholines         305    15354
## 6         (2-hydroxyethyl)trimethylazanium           Choline C5H14NO   104.1075 Organic nitrogen compounds   Cholines  Cholines         305    15354
##       HMDB_ID LM_ID KEGG_ID                   INCHI_KEY RefMet_ID
## 1 HMDB0000072     -  C00417 GTZCVFVGUGFEME-IWQZZHSRSA-N RM0021619
## 2 HMDB0000072     -  C00417 GTZCVFVGUGFEME-IWQZZHSRSA-N RM0021619
## 3 HMDB0000072     -  C00417 GTZCVFVGUGFEME-IWQZZHSRSA-N RM0021619
## 4 HMDB0000097     -  C00114 OEYIOHPDSNJKLS-UHFFFAOYSA-N RM0028739
## 5 HMDB0000097     -  C00114 OEYIOHPDSNJKLS-UHFFFAOYSA-N RM0028739
## 6 HMDB0000097     -  C00114 OEYIOHPDSNJKLS-UHFFFAOYSA-N RM0028739
```

### Step 2: Identify missing compounds

Next, we identify labels that could not be classified in the initial RefMet query.


``` r
# Filter out entries with missing standardized names (marked as "-")
filtered_refmet <- subset(refmet, Standardized.name == "-")

# View the data
View(filtered_refmet)
```

### Step 3: Manually add missing compounds

We use the addRef function to merge curated data from ../metabolite_reference into the RefMet results, addressing gaps in classifications.


``` r
addRef <- function(data_frame, met_file) {
  
  met <- read.csv(met_file)
  
  ref <- data.frame(
    Input.name = met$sys_name,
    Standardized.name = met$name,
    Formula = met$formula,
    Exact.mass = met$exactmass,
    Super.class = met$cf_superclass,
    Main.class = met$cf_class,
    Sub.class = met$cf_subclass,
    PubChem_CID = met$pubchem_cid,
    ChEBI_ID = met$chebi_id,
    HMDB_ID = met$hmdb_id,
    LM_ID = met$lm_id,
    KEGG_ID = met$kegg_id,
    INCHI_KEY = met$inchi_key,
    RefMet_ID = met$regno
  )
  
  data_frame <- data_frame |>
  left_join(ref, by = "Input.name", suffix = c("_original", "_ref")) |>
  mutate(across(
    ends_with("_original"),
    ~ ifelse(. %in% c("-", NA), get(sub("_original", "_ref", cur_column())), .),
    .names = "{.col}"
  )) |>
  rename_with(~ sub("_original", "", .), ends_with("_original")) |>
  select(names(data_frame))

  return(data_frame)  
}

refmet <- addRef(refmet, "../metabolite_reference/MWSD148427.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD148427.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use DL-a- hydroxyglutaric acid to represent (R)-(-)-citramalic acid/DL-a- hydroxyglutaric acid
# due to higher natural abundance
refmet[16, 1] <- "(3R)-3-hydroxy-4-(trimethylazaniumyl)butanoate"
refmet[17, 1] <- "(3R)-3-hydroxy-4-(trimethylazaniumyl)butanoate"
refmet[18, 1] <- "(3R)-3-hydroxy-4-(trimethylazaniumyl)butanoate"
refmet <- addRef(refmet, "../metabolite_reference/MWSD42914.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD42914.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use gamma(amino)-butyric acid to represent 2-aminobutyrate/gamma(amino)-butyric acid
# due to higher natural abundance
refmet[43, 1] <- "4-amino-butanoic acid"
refmet[44, 1] <- "4-amino-butanoic acid"
refmet[45, 1] <- "4-amino-butanoic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD1864.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD1864.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use 2-hydroxypropane-1,2,3-tricarboxylic acid to represent 2-hydroxypropane-1,2,3-tricarboxylic acid/isocitrate
# due to higher natural abundance
refmet[58, 1] <- "2-hydroxypropane-1,2,3-tricarboxylic acid"
refmet[59, 1] <- "2-hydroxypropane-1,2,3-tricarboxylic acid"
refmet[60, 1] <- "2-hydroxypropane-1,2,3-tricarboxylic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37071.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37071.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use 3,4-Dihydroxyphenylacetate to represent 3,4-Dihydroxyphenylacetate/Homogentisate
# due to higher natural abundance
refmet[73, 1] <- "2-(3,4-dihydroxyphenyl)acetic acid"
refmet[74, 1] <- "2-(3,4-dihydroxyphenyl)acetic acid"
refmet[75, 1] <- "2-(3,4-dihydroxyphenyl)acetic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37733.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37733.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use 4-hydroxyphenylacetate to represent 3-hydroxyphenylacetate/4-hydroxyphenylacetate
# due to higher natural abundance
refmet[82, 1] <- "4-hydroxyphenyl acetate"
refmet[83, 1] <- "4-hydroxyphenyl acetate"
refmet[84, 1] <- "4-hydroxyphenyl acetate"
refmet <- addRef(refmet, "../metabolite_reference/MWSD54341.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD54341.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use pimelate to represent 3-methyladipic acid/pimelate (heptanedioate)
# due to higher natural abundance
refmet[88, 1] <- "Heptanedioic acid"
refmet[89, 1] <- "Heptanedioic acid"
refmet[90, 1] <- "Heptanedioic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD1973.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD1973.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use Norepinephrine to represent 4,5-bis(hydroxymethyl)-2-methylpyridin-3-ol/Norepinephrine (noradrenaline)
# due to higher natural abundance
refmet[91, 1] <- "4-[(1R)-2-amino-1-hydroxyethyl]benzene-1,2-diol"
refmet[92, 1] <- "4-[(1R)-2-amino-1-hydroxyethyl]benzene-1,2-diol"
refmet[93, 1] <- "4-[(1R)-2-amino-1-hydroxyethyl]benzene-1,2-diol"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37141.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37141.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use 4-Aminobenzoate to represent 4-Aminobenzoate/anthranilate
# due to higher natural abundance
refmet[97, 1] <- "4-aminobenzoic acid"
refmet[98, 1] <- "4-aminobenzoic acid"
refmet[99, 1] <- "4-aminobenzoic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37770.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37770.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use 4'-Hydroxyflavanone to represent 4'-Hydroxyflavanone/6-Hydroxyflavanone
# due to higher natural abundance
refmet[103, 1] <- "(2S)-2-(4-hydroxyphenyl)-2,3-dihydro-4H-chromen-4-one"
refmet[104, 1] <- "(2S)-2-(4-hydroxyphenyl)-2,3-dihydro-4H-chromen-4-one"
refmet[105, 1] <- "(2S)-2-(4-hydroxyphenyl)-2,3-dihydro-4H-chromen-4-one"
refmet <- addRef(refmet, "../metabolite_reference/MWSD56440.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD56440.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use thymine to represent 4-imidazoleacetate/thymine
# due to higher natural abundance
refmet[106, 1] <- "5-methyl-1,2,3,4-tetrahydropyrimidine-2,4-dione"
refmet[107, 1] <- "5-methyl-1,2,3,4-tetrahydropyrimidine-2,4-dione"
refmet[108, 1] <- "5-methyl-1,2,3,4-tetrahydropyrimidine-2,4-dione"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37168.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37168.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use L-Hydroxyproline to represent 5-Aminolevulinate/L-Hydroxyproline
# due to higher natural abundance
refmet[109, 1] <- "(4S)-4-hydroxy-L-proline"
refmet[110, 1] <- "(4S)-4-hydroxy-L-proline"
refmet[111, 1] <- "(4S)-4-hydroxy-L-proline"
refmet <- addRef(refmet, "../metabolite_reference/MWSD51705.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD51705.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use L-valine to represent 5-aminovalerate/L-valine
# due to higher natural abundance
refmet[112, 1] <- "(2S)-2-amino-3-methylbutanoic acid"
refmet[113, 1] <- "(2S)-2-amino-3-methylbutanoic acid"
refmet[114, 1] <- "(2S)-2-amino-3-methylbutanoic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37484.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37484.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[118, 1] <- "(2S)-2-amino-4-{[(1R)-1-[(carboxymethyl)carbamoyl]-2-sulfanylethyl]carbamoyl}butanoic acid"
refmet[119, 1] <- "(2S)-2-amino-4-{[(1R)-1-[(carboxymethyl)carbamoyl]-2-sulfanylethyl]carbamoyl}butanoic acid"
refmet[120, 1] <- "(2S)-2-amino-4-{[(1R)-1-[(carboxymethyl)carbamoyl]-2-sulfanylethyl]carbamoyl}butanoic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37087.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37087.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# L-leucine to represent 6-Aminohexanoate/L-isoleucine/L-leucine
# with not particular reason but should aware it could be L-isoleucine as well
refmet[130, 1] <- "(2S)-2-amino-4-methylpentanoic acid"
refmet[131, 1] <- "(2S)-2-amino-4-methylpentanoic acid"
refmet[132, 1] <- "(2S)-2-amino-4-methylpentanoic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD42493.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD42493.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use Adenosine to represent Adenosine/Deoxyguanosine
# due to higher natural abundance
refmet[145, 1] <- "(2R,3R,4S,5R)-2-(6-amino-9H-purin-9-yl)-5-(hydroxymethyl)oxolane-3,4-diol"
refmet[146, 1] <- "(2R,3R,4S,5R)-2-(6-amino-9H-purin-9-yl)-5-(hydroxymethyl)oxolane-3,4-diol"
refmet[147, 1] <- "(2R,3R,4S,5R)-2-(6-amino-9H-purin-9-yl)-5-(hydroxymethyl)oxolane-3,4-diol"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37045.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37045.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use alpha-D-Glucose 6-phosphate to represent adenosine diphosphate/dGDP
# due to higher natural abundance
refmet[148, 1] <- "[({[(2R,3S,4R,5R)-5-(6-amino-9H-purin-9-yl)-3,4-dihydroxyoxolan-2-yl]methoxy}(hydroxy)phosphoryl)oxy]phosphonic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37737.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37737.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use alpha-D-Glucose 6-phosphate to represent alpha-D-Glucose 6-phosphate/beta-D-Fructose 6-phosphate/glucose 1-phosphate
# due to highest natural abundance
refmet[155, 1] <- "alpha-D-glucopyranose 6-(dihydrogen phosphate)"
refmet[156, 1] <- "alpha-D-glucopyranose 6-(dihydrogen phosphate)"
refmet[157, 1] <- "alpha-D-glucopyranose 6-(dihydrogen phosphate)"
refmet <- addRef(refmet, "../metabolite_reference/MWSD50994.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD50994.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use CHCA to represent CHCA(alpha-Cyano-4-hydroxycinnamic acid)/kynurenate
# due to its use as matrix deposition compound
refmet[179, 1] <- "(2E)-2-cyano-3-(4-hydroxyphenyl)prop-2-enoic acid"
refmet[180, 1] <- "(2E)-2-cyano-3-(4-hydroxyphenyl)prop-2-enoic acid"
refmet[181, 1] <- "(2E)-2-cyano-3-(4-hydroxyphenyl)prop-2-enoic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD63334.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD63334.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use Chlorogenic acid to represent Chlorogenic acid/Neochlorogenic acid
# due to higher natural abundance
refmet[182, 1] <- "(1S,3R,4R,5R)-3-{[(2E)-3-(3,4-dihydroxyphenyl)prop-2-enoyl]oxy}-1,4,5-trihydroxycyclohexane-1-carboxylic acid"
refmet[183, 1] <- "(1S,3R,4R,5R)-3-{[(2E)-3-(3,4-dihydroxyphenyl)prop-2-enoyl]oxy}-1,4,5-trihydroxycyclohexane-1-carboxylic acid"
refmet[184, 1] <- "(1S,3R,4R,5R)-3-{[(2E)-3-(3,4-dihydroxyphenyl)prop-2-enoyl]oxy}-1,4,5-trihydroxycyclohexane-1-carboxylic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD38262.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD38262.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[220, 1] <- "(2R)-2-hydroxy-3-oxopropyl dihydrogen phosphate"
refmet[221, 1] <- "(2R)-2-hydroxy-3-oxopropyl dihydrogen phosphate"
refmet[222, 1] <- "(2R)-2-hydroxy-3-oxopropyl dihydrogen phosphate"
refmet <- addRef(refmet, "../metabolite_reference/MWSD51939.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD51939.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[226, 1] <- "(2R,3R)-2-(3,4-dihydroxyphenyl)-3,4-dihydro-2H-1-benzopyran-3,5,7-triol"
refmet[227, 1] <- "(2R,3R)-2-(3,4-dihydroxyphenyl)-3,4-dihydro-2H-1-benzopyran-3,5,7-triol"
refmet[228, 1] <- "(2R,3R)-2-(3,4-dihydroxyphenyl)-3,4-dihydro-2H-1-benzopyran-3,5,7-triol"
refmet <- addRef(refmet, "../metabolite_reference/MWSD21833.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD21833.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[229, 1] <- "[(2R,3R)-2-(3,4-dihydroxyphenyl)-5,7-dihydroxy-3,4-dihydro-2H-chromen-3-yl] 3,4,5-trihydroxybenzoate"
refmet <- addRef(refmet, "../metabolite_reference/MWSD21920.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD21920.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[230, 1] <- "(2R,3R)-2-(3,4,5-trihydroxyphenyl)-3,4-dihydro-2H-chromene-3,5,7-triol"
refmet[231, 1] <- "(2R,3R)-2-(3,4,5-trihydroxyphenyl)-3,4-dihydro-2H-chromene-3,5,7-triol"
refmet[232, 1] <- "(2R,3R)-2-(3,4,5-trihydroxyphenyl)-3,4-dihydro-2H-chromene-3,5,7-triol"
refmet <- addRef(refmet, "../metabolite_reference/MWSD21834.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD21834.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[233, 1] <- "6,7,13,14-tetrahydroxy-2,9-dioxatetracyclo[6.6.2.0^{4,16}.0^{11,15}]hexadeca-1(15),4,6,8(16),11,13-hexaene-3,10-dione"
refmet[234, 1] <- "6,7,13,14-tetrahydroxy-2,9-dioxatetracyclo[6.6.2.0^{4,16}.0^{11,15}]hexadeca-1(15),4,6,8(16),11,13-hexaene-3,10-dione"
refmet[235, 1] <- "6,7,13,14-tetrahydroxy-2,9-dioxatetracyclo[6.6.2.0^{4,16}.0^{11,15}]hexadeca-1(15),4,6,8(16),11,13-hexaene-3,10-dione"
refmet <- addRef(refmet, "../metabolite_reference/MWSD38219.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD38219.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use glutarate to represent Ethylmalonic acid/glutarate (pentanedioate)
# due to higher natural abundance
refmet[242, 1] <- "pentanedioic acid"
refmet[243, 1] <- "pentanedioic acid"
refmet[244, 1] <- "pentanedioic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37356.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37356.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use Kaempferol to represent Fisetin/Kaempferol
# due to higher natural abundance
refmet[245, 1] <- "3,5,7-trihydroxy-2-(4-hydroxyphenyl)chromen-4-one"
refmet[246, 1] <- "3,5,7-trihydroxy-2-(4-hydroxyphenyl)chromen-4-one"
refmet[247, 1] <- "3,5,7-trihydroxy-2-(4-hydroxyphenyl)chromen-4-one"
refmet <- addRef(refmet, "../metabolite_reference/MWSD23088.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD23088.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use Glutamic acid to represent Glutamic acid/O-acetylserine
# due to higher natural abundance
refmet[255, 1] <- "(2S)-2-aminopentanedioic acid"
refmet[256, 1] <- "(2S)-2-aminopentanedioic acid"
refmet[257, 1] <- "(2S)-2-aminopentanedioic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37101.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37101.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[283, 1] <- "(2S)-2-amino-3-(1H-imidazol-4-yl)propanoic acid"
refmet[284, 1] <- "(2S)-2-amino-3-(1H-imidazol-4-yl)propanoic acid"
refmet[285, 1] <- "(2S)-2-amino-3-(1H-imidazol-4-yl)propanoic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37119.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37119.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[328, 1] <- "(2S)-2-amino-4-sulfanylbutanoic acid"
refmet[329, 1] <- "(2S)-2-amino-4-sulfanylbutanoic acid"
refmet[330, 1] <- "(2S)-2-amino-4-sulfanylbutanoic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD50967.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD50967.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use L-threonine to represent L-Homoserine/L-threonine 
# due to higher natural abundance in mammal
refmet[331, 1] <- "(2S,3R)-2-amino-3-hydroxybutanoic acid"
refmet[332, 1] <- "(2S,3R)-2-amino-3-hydroxybutanoic acid"
refmet[333, 1] <- "(2S,3R)-2-amino-3-hydroxybutanoic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37113.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37113.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[370, 1] <- "N-[4-(3-acetamidopropylamino)butyl]acetamide"
refmet[371, 1] <- "N-[4-(3-acetamidopropylamino)butyl]acetamide"
refmet[372, 1] <- "N-[4-(3-acetamidopropylamino)butyl]acetamide"
refmet <- addRef(refmet, "../metabolite_reference/MWSD67065.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD67065.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use N1-Acetylspermidine to represent N1-Acetylspermidine/N8-Acetylspermidine 
# due to higher natural abundance
refmet[376, 1] <- "N-{3-[(4-aminobutyl)amino]propyl}acetamide"
refmet[377, 1] <- "N-{3-[(4-aminobutyl)amino]propyl}acetamide"
refmet[378, 1] <- "N-{3-[(4-aminobutyl)amino]propyl}acetamide"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37703.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37703.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use uridine to represent pseudouridine/uridine 
# due to higher natural abundance
refmet[456, 1] <- "1-[(2R,3R,4S,5R)-3,4-dihydroxy-5-(hydroxymethyl)oxolan-2-yl]-1,2,3,4-tetrahydropyrimidine-2,4-dione"
refmet[457, 1] <- "1-[(2R,3R,4S,5R)-3,4-dihydroxy-5-(hydroxymethyl)oxolan-2-yl]-1,2,3,4-tetrahydropyrimidine-2,4-dione"
refmet[458, 1] <- "1-[(2R,3R,4S,5R)-3,4-dihydroxy-5-(hydroxymethyl)oxolan-2-yl]-1,2,3,4-tetrahydropyrimidine-2,4-dione"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37190.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37190.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[459, 1] <- "4-{[(2-amino-4-hydroxypteridin-6-yl)methyl]amino}benzoic acid"
refmet[460, 1] <- "4-{[(2-amino-4-hydroxypteridin-6-yl)methyl]amino}benzoic acid"
refmet[461, 1] <- "4-{[(2-amino-4-hydroxypteridin-6-yl)methyl]amino}benzoic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD51394.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD51394.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use xylitol to represent ribitol (adonitol)/xylitol 
# due to higher natural abundance
refmet[480, 1] <- "(2R,3R,4S)-Pentane-1,2,3,4,5-pentol"
refmet[481, 1] <- "(2R,3R,4S)-Pentane-1,2,3,4,5-pentol"
refmet[482, 1] <- "(2R,3R,4S)-Pentane-1,2,3,4,5-pentol"
refmet <- addRef(refmet, "../metabolite_reference/MWSD38221.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD38221.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Use ribose 5-phosphate to represent ribose 5-phosphate/Xylulose 5-phosphate 
# due to higher natural abundance
refmet[483, 1] <- "{[(2R,3S,4R)-3,4,5-trihydroxyoxolan-2-yl]methoxy}phosphonic acid"
refmet[484, 1] <- "{[(2R,3S,4R)-3,4,5-trihydroxyoxolan-2-yl]methoxy}phosphonic acid"
refmet[485, 1] <- "{[(2R,3S,4R)-3,4,5-trihydroxyoxolan-2-yl]methoxy}phosphonic acid"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37864.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37864.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[489, 1] <- "[(3R)-3-amino-3-carboxypropyl]({[(2S,3S,4R,5R)-5-(6-amino-9H-purin-9-yl)-3,4-dihydroxyoxolan-2-yl]methyl})methylsulfanium"
refmet[490, 1] <- "[(3R)-3-amino-3-carboxypropyl]({[(2S,3S,4R,5R)-5-(6-amino-9H-purin-9-yl)-3,4-dihydroxyoxolan-2-yl]methyl})methylsulfanium"
refmet[491, 1] <- "[(3R)-3-amino-3-carboxypropyl]({[(2S,3S,4R,5R)-5-(6-amino-9H-purin-9-yl)-3,4-dihydroxyoxolan-2-yl]methyl})methylsulfanium"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37647.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37647.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[501, 1] <- "(2R,3R,4R,5S)-hexane-1,2,3,4,5,6-hexol"
refmet[502, 1] <- "(2R,3R,4R,5S)-hexane-1,2,3,4,5,6-hexol"
refmet[503, 1] <- "(2R,3R,4R,5S)-hexane-1,2,3,4,5,6-hexol"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37159.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37159.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[517, 1] <- "3-[(4-amino-2-methylpyrimidin-5-yl)methyl]-5-(2-hydroxyethyl)-4-methyl-1,3-thiazol-3-ium"
refmet[518, 1] <- "3-[(4-amino-2-methylpyrimidin-5-yl)methyl]-5-(2-hydroxyethyl)-4-methyl-1,3-thiazol-3-ium"
refmet[519, 1] <- "3-[(4-amino-2-methylpyrimidin-5-yl)methyl]-5-(2-hydroxyethyl)-4-methyl-1,3-thiazol-3-ium"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37152.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37152.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[520, 1] <- "2-[3-[(4-amino-2-methyl-pyrimidin-5-yl)methyl]-4-methyl-thiazol-3-ium-5-yl]ethyl phosphono hydrogen phosphate"
refmet[521, 1] <- "2-[3-[(4-amino-2-methyl-pyrimidin-5-yl)methyl]-4-methyl-thiazol-3-ium-5-yl]ethyl phosphono hydrogen phosphate"
refmet <- addRef(refmet, "../metabolite_reference/MWSD67409.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD67409.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
refmet[537, 1] <- "7H-purin-6-amine"
refmet[538, 1] <- "7H-purin-6-amine"
refmet[539, 1] <- "7H-purin-6-amine"
refmet <- addRef(refmet, "../metabolite_reference/MWSD37038.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD37038.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Check missing data
filtered_refmet <- subset(refmet, Standardized.name == "-")

# Preview the data
head(filtered_refmet)
```

```
##                                        Input.name Standardized.name Formula Exact.mass Super.class Main.class Sub.class PubChem_CID ChEBI_ID HMDB_ID
## 10 (3R)-3-hydroxy-4-(trimethylazaniumyl)butanoate                 -       -          -           -          -         -           -        -       -
## 11 (3R)-3-hydroxy-4-(trimethylazaniumyl)butanoate                 -       -          -           -          -         -           -        -       -
## 12 (3R)-3-hydroxy-4-(trimethylazaniumyl)butanoate                 -       -          -           -          -         -           -        -       -
## 16 (3R)-3-hydroxy-4-(trimethylazaniumyl)butanoate                 -       -          -           -          -         -           -        -       -
## 17 (3R)-3-hydroxy-4-(trimethylazaniumyl)butanoate                 -       -          -           -          -         -           -        -       -
## 18 (3R)-3-hydroxy-4-(trimethylazaniumyl)butanoate                 -       -          -           -          -         -           -        -       -
##    LM_ID KEGG_ID INCHI_KEY RefMet_ID
## 10     -       -         -         -
## 11     -       -         -         -
## 12     -       -         -         -
## 16     -       -         -         -
## 17     -       -         -         -
## 18     -       -         -         -
```

``` r
filtered_refmet <- subset(refmet, is.na(Standardized.name))
# Preview the data
head(filtered_refmet)
```

```
##  [1] Input.name        Standardized.name Formula           Exact.mass        Super.class       Main.class        Sub.class         PubChem_CID      
##  [9] ChEBI_ID          HMDB_ID           LM_ID             KEGG_ID           INCHI_KEY         RefMet_ID        
## <0 rows> (or 0-length row.names)
```

### Step 4: Reclassify the dataset

With updated labels, we re-run the classification step to ensure improved accuracy.


``` r
new_tag <- tag
new_tag$Label <- refmet$Standardized.name

met_list <- stri_join_list(list(new_tag$Label), sep = "\n")
h <- new_handle()
handle_setform(h, metabolite_name = met_list)
req <- curl_fetch_memory("https://www.metabolomicsworkbench.org/databases/refmet/name_to_refmet_new_minID.php", handle = h)

new_refmet <- read.table(
  text = rawToChar(req$content),
  header = TRUE,
  na.strings = "-",
  stringsAsFactors = FALSE,
  quote = "",
  comment.char = "",
  sep = "\t"
)

new_refmet[is.na(new_refmet)] <- "-"
new_refmet[new_refmet == ""] <- "-"

# Preview the data
head(new_refmet)
```

```
##          Input.name Standardized.name Formula Exact.mass                Super.class Main.class Sub.class PubChem_CID ChEBI_ID     HMDB_ID LM_ID
## 1 cis-Aconitic acid cis-Aconitic acid  C6H6O6   174.0164              Organic acids  TCA acids TCA acids      643757    32805 HMDB0000072     -
## 2 cis-Aconitic acid cis-Aconitic acid  C6H6O6   174.0164              Organic acids  TCA acids TCA acids      643757    32805 HMDB0000072     -
## 3 cis-Aconitic acid cis-Aconitic acid  C6H6O6   174.0164              Organic acids  TCA acids TCA acids      643757    32805 HMDB0000072     -
## 4           Choline           Choline C5H14NO   104.1075 Organic nitrogen compounds   Cholines  Cholines         305    15354 HMDB0000097     -
## 5           Choline           Choline C5H14NO   104.1075 Organic nitrogen compounds   Cholines  Cholines         305    15354 HMDB0000097     -
## 6           Choline           Choline C5H14NO   104.1075 Organic nitrogen compounds   Cholines  Cholines         305    15354 HMDB0000097     -
##   KEGG_ID                   INCHI_KEY RefMet_ID
## 1  C00417 GTZCVFVGUGFEME-IWQZZHSRSA-N RM0021619
## 2  C00417 GTZCVFVGUGFEME-IWQZZHSRSA-N RM0021619
## 3  C00417 GTZCVFVGUGFEME-IWQZZHSRSA-N RM0021619
## 4  C00114 OEYIOHPDSNJKLS-UHFFFAOYSA-N RM0028739
## 5  C00114 OEYIOHPDSNJKLS-UHFFFAOYSA-N RM0028739
## 6  C00114 OEYIOHPDSNJKLS-UHFFFAOYSA-N RM0028739
```

``` r
# Check missing data
filtered_refmet <- subset(new_refmet, Standardized.name == "-")

# Preview the data
head(filtered_refmet)
```

```
##    Input.name Standardized.name Formula Exact.mass Super.class Main.class Sub.class PubChem_CID ChEBI_ID HMDB_ID LM_ID KEGG_ID INCHI_KEY RefMet_ID
## 10          -                 -       -          -           -          -         -           -        -       -     -       -         -         -
## 11          -                 -       -          -           -          -         -           -        -       -     -       -         -         -
## 12          -                 -       -          -           -          -         -           -        -       -     -       -         -         -
## 16          -                 -       -          -           -          -         -           -        -       -     -       -         -         -
## 17          -                 -       -          -           -          -         -           -        -       -     -       -         -         -
## 18          -                 -       -          -           -          -         -           -        -       -     -       -         -         -
```

``` r
filtered_refmet <- subset(new_refmet, is.na(Standardized.name))
# Preview the data
head(filtered_refmet)
```

```
##  [1] Input.name        Standardized.name Formula           Exact.mass        Super.class       Main.class        Sub.class         PubChem_CID      
##  [9] ChEBI_ID          HMDB_ID           LM_ID             KEGG_ID           INCHI_KEY         RefMet_ID        
## <0 rows> (or 0-length row.names)
```

### Step 5: Fill remaining classification gaps manually

Finally, we manually address any residual gaps by filling in missing classifications directly.Fill out the missing class


``` r
new_refmet[19, 1] <- "[2-(trimethylazaniumyl)ethoxy]phosphonic acid"
new_refmet[20, 1] <- "[2-(trimethylazaniumyl)ethoxy]phosphonic acid"
new_refmet[21, 1] <- "[2-(trimethylazaniumyl)ethoxy]phosphonic acid"
new_refmet <- addRef(new_refmet, "../metabolite_reference/MWSD148427.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD148427.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
new_refmet[103, 1] <- "(2S)-2-(4-hydroxyphenyl)-2,3-dihydro-4H-chromen-4-one"
new_refmet[104, 1] <- "(2S)-2-(4-hydroxyphenyl)-2,3-dihydro-4H-chromen-4-one"
new_refmet[105, 1] <- "(2S)-2-(4-hydroxyphenyl)-2,3-dihydro-4H-chromen-4-one"
new_refmet <- addRef(new_refmet, "../metabolite_reference/MWSD56440.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD56440.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
new_refmet[155, 1] <- "alpha-D-glucopyranose 6-(dihydrogen phosphate)"
new_refmet[156, 1] <- "alpha-D-glucopyranose 6-(dihydrogen phosphate)"
new_refmet[157, 1] <- "alpha-D-glucopyranose 6-(dihydrogen phosphate)"
new_refmet <- addRef(new_refmet, "../metabolite_reference/MWSD50994.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD50994.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
new_refmet[179, 1] <- "(2E)-2-cyano-3-(4-hydroxyphenyl)prop-2-enoic acid"
new_refmet[180, 1] <- "(2E)-2-cyano-3-(4-hydroxyphenyl)prop-2-enoic acid"
new_refmet[181, 1] <- "(2E)-2-cyano-3-(4-hydroxyphenyl)prop-2-enoic acid"
new_refmet <- addRef(new_refmet, "../metabolite_reference/MWSD63334.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD63334.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
new_refmet[459, 1] <- "4-{[(2-amino-4-hydroxypteridin-6-yl)methyl]amino}benzoic acid"
new_refmet[460, 1] <- "4-{[(2-amino-4-hydroxypteridin-6-yl)methyl]amino}benzoic acid"
new_refmet[461, 1] <- "4-{[(2-amino-4-hydroxypteridin-6-yl)methyl]amino}benzoic acid"
new_refmet <- addRef(new_refmet, "../metabolite_reference/MWSD51394.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../metabolite_reference/MWSD51394.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
# Preview the data
head(new_refmet)
```

```
##          Input.name Standardized.name Formula Exact.mass                Super.class Main.class Sub.class PubChem_CID ChEBI_ID     HMDB_ID LM_ID
## 1 cis-Aconitic acid cis-Aconitic acid  C6H6O6   174.0164              Organic acids  TCA acids TCA acids      643757    32805 HMDB0000072     -
## 2 cis-Aconitic acid cis-Aconitic acid  C6H6O6   174.0164              Organic acids  TCA acids TCA acids      643757    32805 HMDB0000072     -
## 3 cis-Aconitic acid cis-Aconitic acid  C6H6O6   174.0164              Organic acids  TCA acids TCA acids      643757    32805 HMDB0000072     -
## 4           Choline           Choline C5H14NO   104.1075 Organic nitrogen compounds   Cholines  Cholines         305    15354 HMDB0000097     -
## 5           Choline           Choline C5H14NO   104.1075 Organic nitrogen compounds   Cholines  Cholines         305    15354 HMDB0000097     -
## 6           Choline           Choline C5H14NO   104.1075 Organic nitrogen compounds   Cholines  Cholines         305    15354 HMDB0000097     -
##   KEGG_ID                   INCHI_KEY RefMet_ID
## 1  C00417 GTZCVFVGUGFEME-IWQZZHSRSA-N RM0021619
## 2  C00417 GTZCVFVGUGFEME-IWQZZHSRSA-N RM0021619
## 3  C00417 GTZCVFVGUGFEME-IWQZZHSRSA-N RM0021619
## 4  C00114 OEYIOHPDSNJKLS-UHFFFAOYSA-N RM0028739
## 5  C00114 OEYIOHPDSNJKLS-UHFFFAOYSA-N RM0028739
## 6  C00114 OEYIOHPDSNJKLS-UHFFFAOYSA-N RM0028739
```

``` r
# Check missing data
filtered_refmet <- subset(new_refmet, Standardized.name == "-")

# Preview the data
head(filtered_refmet)
```

```
##    Input.name Standardized.name Formula Exact.mass Super.class Main.class Sub.class PubChem_CID ChEBI_ID HMDB_ID LM_ID KEGG_ID INCHI_KEY RefMet_ID
## 10          -                 -       -          -           -          -         -           -        -       -     -       -         -         -
## 11          -                 -       -          -           -          -         -           -        -       -     -       -         -         -
## 12          -                 -       -          -           -          -         -           -        -       -     -       -         -         -
## 16          -                 -       -          -           -          -         -           -        -       -     -       -         -         -
## 17          -                 -       -          -           -          -         -           -        -       -     -       -         -         -
## 18          -                 -       -          -           -          -         -           -        -       -     -       -         -         -
```

``` r
filtered_refmet <- subset(new_refmet, is.na(Standardized.name))
# Preview the data
head(filtered_refmet)
```

```
##  [1] Input.name        Standardized.name Formula           Exact.mass        Super.class       Main.class        Sub.class         PubChem_CID      
##  [9] ChEBI_ID          HMDB_ID           LM_ID             KEGG_ID           INCHI_KEY         RefMet_ID        
## <0 rows> (or 0-length row.names)
```

At this step, we save the final refined RefMet classification dataset to a CSV file. This serves as a clear checkpoint, ensuring the data is preserved and avoiding any mix-ups if you plan to analyze a different data matrix in the future.


``` r
# Save the reference for future use
write.csv(new_refmet, file = "../../Results/4C_brain/refmet_brain.csv", row.names = FALSE)
```

```
## Warning in file(file, ifelse(append, "a", "w")): cannot open file '../../Results/4C_brain/refmet_brain.csv': No such file or directory
```

```
## Error in file(file, ifelse(append, "a", "w")): cannot open the connection
```

Now that we’ve refined the classifications with RefMet, it’s time to integrate this information into our dataset. In this step, we update the Label column in new_tag with the Standardized.name from the refined RefMet results and add hierarchical classifications, including Superclass, Mainclass, and Subclass. To ensure consistency, we clean these classification columns by removing unnecessary annotations (like text in square brackets) and trimming whitespace. Finally, we merge this enriched new_tag dataset with the cluster data, creating a comprehensive dataset that combines refined metabolite information and cluster associations, ready for further analysis.


``` r
# Add classifications to `new_tag`
new_refmet <- read.csv("../../Results/4C_brain/refmet_brain.csv")
```

```
## Warning in file(file, "rt"): cannot open file '../../Results/4C_brain/refmet_brain.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

``` r
new_tag <- tag
new_tag$Label <- new_refmet$Standardized.name
new_tag$Superclass <- new_refmet$Super.class
new_tag$Mainclass <- new_refmet$Main.class
new_tag$Subclass <- new_refmet$Sub.class

# Clean classification columns
columns_to_clean <- c("Superclass", "Mainclass", "Subclass")
new_tag[columns_to_clean] <- lapply(new_tag[columns_to_clean], function(column) {
  cleaned_column <- gsub("\\[.*?\\]", "", column) 
  trimws(cleaned_column) 
})

# Merge the enriched tag data with the cluster data
merged_cluster <- merge(new_tag, cluster)

# Preview data
head(merged_cluster)
```

```
##        m.z                              Label    Superclass                Mainclass        Subclass Intensity.cluster1 Intensity.cluster2
## 1  76.0393                            Glycine Organic acids Amino acids and peptides     Amino acids          0.1442761          0.3929302
## 2  89.0597                       Butyric acid   Fatty Acyls              Fatty acids    Saturated FA          0.2226788          0.1632750
## 3  89.1073                         Putrescine   Fatty Acyls             Fatty amines Monoalkylamines          0.3082936          0.5927600
## 4  90.0550                            Alanine Organic acids Amino acids and peptides     Amino acids         83.5216220        105.4862700
## 5  98.0212                            Glycine Organic acids Amino acids and peptides     Amino acids          0.0000000          0.0000000
## 6 102.0550 1-Aminocyclopropanecarboxylic acid Organic acids Amino acids and peptides     Amino acids          0.2973465          0.3293756
##   Intensity.cluster3 Intensity.cluster4 Intensity.cluster5 Intensity.cluster6 Intensity.cluster7 Intensity.cluster8 Intensity.cluster9
## 1          0.0299838          0.1837246          0.2185196          0.2355051          1.0348138          0.1940557          0.6497177
## 2          0.2282751          0.1963871          0.0849793          0.1453071          0.0908355          0.1508301          0.3000713
## 3          0.0000000          0.2868329          0.5273454          0.5735760          0.7420870          0.4632995          0.7388196
## 4         63.4240346        108.4669570        125.7119275        280.5406887         83.9715056        129.1564853         69.4796682
## 5          0.0000000          0.0000000          0.0000000          0.0000000          0.0000000          0.0000000          0.0000000
## 6          0.3526377          0.3527773          0.2875921          0.1647493          0.2625769          0.3776691          0.1509223
##   Intensity.cluster10 Intensity.cluster11 Intensity.cluster12 Intensity.cluster13 Intensity.cluster14 Intensity.cluster15 Intensity.cluster16
## 1           0.0964330           0.1505571           0.1034768           0.2523033           0.1906133           1.3934011           0.3281161
## 2           0.1742996           0.1917549           0.2135133           0.3444325           0.1298350           0.1737150           0.0637115
## 3           0.3287836           0.4249475           0.0000000           0.3884697           0.3318152           0.5117637           0.6931002
## 4         145.1680149          85.0197596          43.6503745          95.0312664          92.6605302          55.1248950         212.1843165
## 5           0.0000000           0.0000000           0.0000000           0.0000000           0.0000000           0.0000000           0.0000000
## 6           0.2640540           0.2511292           0.4504674           0.3629882           0.3287221           0.1936912           0.1660350
##   Intensity.cluster17 Intensity.cluster18 Intensity.cluster19 Intensity.cluster20
## 1           0.5503210           0.3399630           0.2504313           0.7239582
## 2           0.1825224           0.1561711           0.0769163           0.1972481
## 3           0.6405906           0.5057812           0.5449819           0.7674770
## 4         177.5875341         165.1214300          77.9882114         100.8368378
## 5           0.0000000           0.0000000           0.0000000           0.0000000
## 6           0.2635150           0.1649331           0.3125222           0.3264301
```

In this step, we prepare the data for visualization by summarizing it based on Superclass and Mainclass. We start by defining a set of unique colors to distinguish between superclasses in the plots. Afterward, we remove unnecessary columns and group the data by Superclass and Mainclass to calculate the total intensity for each group.

The total ion current (TIC) is also computed as a percentage contribution of each group to the overall intensity. To simplify the data for visualization, minor classes within a superclass are labeled as “Other” if their contribution is minimal. Finally, we assign colors to each superclass and its main classes for consistent representation in the plots.


``` r
# Define unique colors for Superclasses
unique_colors <- c("#F8766D", "#DE8C00", "#DEAC00", "#7CAE00", "#00C08B", "#00B4F0", "#619CFF", "#C77CFF", "#F564E3", "#FF64B0")

# Remove unnecessary columns and process data
tmp_df <- merged_cluster[, -c(1, 2, 5)]

# Summarize data for barplot
summarized_for_barplot <- tmp_df |>
    group_by(Superclass, Mainclass) |>
    summarize(across(starts_with("Intensity"), sum, na.rm = TRUE), .groups = "drop") |>
    ungroup() |>
    mutate(
        TIC = rowSums(across(starts_with("Intensity")), na.rm = TRUE),
        TIC = round(TIC / sum(TIC) * 100, 2)
    ) |>
    filter(TIC > 0) |>
    group_by(Superclass) |>
    mutate(
        mainclass.number = n(),
        mainclass.min = (TIC == min(TIC)),
        Mainclass = ifelse(mainclass.number > 1 & mainclass.min,
                           paste0("Other ", tolower(Superclass)), Mainclass),
        Mainclass = ifelse(mainclass.number > 2 & TIC < 1,
                           paste0("Other ", tolower(Superclass)), Mainclass)
    ) |>
    group_by(Superclass, Mainclass) |>
    summarize(across(matches("^Intensity|^TIC"), sum, na.rm = TRUE), .groups = "drop") |>
    arrange(Superclass, desc(TIC), Mainclass) |>
    mutate(
        Superclass.color = unique_colors[match(Superclass, unique(Superclass))],
        Mainclass.color = scales::alpha(Superclass.color, alpha = 0.7 + (row_number() / n()) * 0.2)
    )

# Preview data
head(summarized_for_barplot)
```

```
## # A tibble: 6 × 25
##   Superclass    Mainclass           Intensity.cluster1 Intensity.cluster2 Intensity.cluster3 Intensity.cluster4 Intensity.cluster5 Intensity.cluster6
##   <chr>         <chr>                            <dbl>              <dbl>              <dbl>              <dbl>              <dbl>              <dbl>
## 1 -             -                              4095.              4004.               4569.             3951.              4316.              4079.  
## 2 Alkaloids     Ornithine alkaloids            7828.             10064.                 44.2           13112.              7514.             12884.  
## 3 Alkaloids     Pyridine alkaloids             1473.              1431.               2594.             1378.              1513.              1376.  
## 4 Alkaloids     Other alkaloids                 187.               182.                454.              183.               203.               200.  
## 5 Benzenoids    Benzenes                         15.4               14.5                51.6              16.0               17.0               16.7 
## 6 Carbohydrates Monosaccharides                   6.19               6.16               49.6               6.07               6.63               8.32
## # ℹ 17 more variables: Intensity.cluster7 <dbl>, Intensity.cluster8 <dbl>, Intensity.cluster9 <dbl>, Intensity.cluster10 <dbl>,
## #   Intensity.cluster11 <dbl>, Intensity.cluster12 <dbl>, Intensity.cluster13 <dbl>, Intensity.cluster14 <dbl>, Intensity.cluster15 <dbl>,
## #   Intensity.cluster16 <dbl>, Intensity.cluster17 <dbl>, Intensity.cluster18 <dbl>, Intensity.cluster19 <dbl>, Intensity.cluster20 <dbl>, TIC <dbl>,
## #   Superclass.color <chr>, Mainclass.color <chr>
```

Next, we reshape the summarized data for plotting. The data is converted from wide format to long format, allowing us to map the Cluster and Intensity to the x- and y-axes, respectively. Each bar in the stacked bar plot represents the intensity of a superclass across different clusters.

Using the ggplot2 package, we create a stacked bar plot where colors differentiate superclasses. The y-axis shows the intensity ratio, while the x-axis represents individual clusters. Finally, we save the plot as a PDF for documentation and display it for verification.


