# 🌍 Multidimensional Vulnerability Dashboard

**A Flexible, Open-Source Tool for Sub-National Vulnerability Assessment and Data-Driven Aid Allocation**  
*Developed using R, Shiny, and open GIS tools*

[![Live Dashboard](https://img.shields.io/badge/Live-Dashboard-blue)](https://farhanasarwar.shinyapps.io/dashboard)

## 🧾 Project Summary

This repository hosts the **Vulnerability Index Dashboard**, a replicable, flexible platform designed to assess regional vulnerability using multidimensional indicators. The tool supports decision-making for aid organizations, governments, and researchers seeking to prioritize interventions based on both population needs and contextual vulnerabilities.

**Key features:**
- 📍 **Geospatial precision** at sub-national levels
- 🧠 **Multi-criteria decision-making** via customized weighting
- 📊 **Interactive visualizations** for exploration
- 🔁 **Easily replicable** for any country with relevant data

Originally built as a **case study for Colombia**, this dashboard can be adapted for any geography.

## 📌 Key Functionalities

| Functionality                      | Description                                                                      |
|------------------------------------|----------------------------------------------------------------------------------|
| 🧮 Composite Vulnerability Index   | Aggregates multidimensional indicators into a single score                       |
| ⚖️ Weight Customization            | Sliders to assign relative importance to thematic areas                          |
| 🔄 Data Transformation             | Normalizes data using Z-score, Percentile, Min-Max, etc.                         |
| 🗺️ GIS Mapping                     | Choropleth maps of index scores using uploaded shapefiles                        |
| 👩🏽‍🏫 Targeting Analysis            | Pareto frontier for resource prioritization                                      |
| 📦 Open Source                     | Built in R with open-source libraries and GIS compatibility                      |

## 📐 Methodology: Step-by-Step Breakdown

The dashboard follows a clear, modular pipeline:

### 1. Data Acquisition
You must supply:
- 📄 An **Excel file** with `Data` and `Metadata` sheets.
- 🗂️ A **shapefile (.zip)** representing sub-national boundaries.

### 2. Preprocessing
Performed automatically by the app:
- Drop variables with >70% missing data
- Standardize region codes to merge with the shapefile
- Impute missing values using **k-Nearest Neighbors (kNN)**
- Adjust indicator direction by inverting variables where **higher values = lower vulnerability**

### 3. Normalization (User-Selectable)

| Method             | Description                                | Best Use Case             |
|--------------------|--------------------------------------------|---------------------------|
| **Z-score**        | `(x - μ) / σ`                              | Normal distributions      |
| **Percentile**     | Relative ranking                           | Outlier-resistant         |
| **Min-Max**        | `(x - min) / (max - min)`                    | Preserves spacing         |
| **Quintile**       | Bins into 5 groups                         | Simpler interpretability  |
| **Equal Interval** | Divides range into equal slices            | Uniform data              |

The final output is rescaled to a [0–10] scale across all variables.

### 4. Dimensional Weighting
Thematic dimensions (e.g., Health, Education, Environment) are defined via the `Metadata` sheet. Each dimension receives a user-adjustable weight.

**Formula:**

- Dimension Score (Mean of Normalized Indicators):

$$
\text{Dimension Score} = \frac{1}{n}\sum_{j=1}^{n} \text{Normalized Indicator}_j
$$

- Vulnerability Index (Weighted Sum of Dimension Scores):

$$
\text{Vulnerability Index} = \sum_{i=1}^{m} \left(\text{Weight}_i \times \text{Dimension Score}_i\right)
$$


## Install Dependencies

Open R or RStudio and run:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "sf", "plotly"))
```

## Run the Application

Launch the dashboard using RStudio:

```r
shiny::runApp("app.R")
```

## Usage

- **Upload Data Files:**
  - Click on the “Upload Data File (Excel)” button and select your prepared Excel file.
  - Upload the corresponding shapefile (.zip) using the designated button.

- **Select Merge Variables:**
  - Choose the merge field from both the Data Sheet and the shapefile to combine the datasets.

- **Choose Population Variable:**
  - Specify the variable representing the total population for size adjustment calculations.

- **Configure Transformation Method:**
  - Select from available options (Percentile, Min-Max, Z-score, Quintile, Equal Interval) based on your data distribution.

- **Adjust Dimension Weights:**
  - Use the slider controls to set the importance of each dimension (e.g., Health, Education) to tailor the index to your analysis needs.

- **View Visualizations:**
  - Navigate through tabs to explore interactive maps, data tables, and Pareto analysis outputs.

- **Download Data:**
  - Export processed data, transformed data, and calculated index scores for further analysis or reporting.

## Configuration & Customization

- **Adapting to Different Geographies:**
  - Update the Excel data file and metadata to match the local context (e.g., new indicators, different merge variables).
  - Redefine variables requiring inversion or size adjustments based on regional priorities.

- **Customization of Visualizations:**
  - Modify color scales, map layers, and popup contents within the code to suit your audience or presentation style.

- **Modular Code Structure:**
  - The repository is organized into separate modules for data preprocessing, transformation, index calculation, and visualization. This structure allows for easy extension and modification.


## Configuration & Customization

- **Adapting to Different Geographies:**
  - Update the Excel data file and metadata to match the local context (e.g., new indicators, different merge variables).
  - Redefine variables requiring inversion or size adjustments based on regional priorities.

- **Customization of Visualizations:**
  - Modify color scales, map layers, and popup contents within the code to suit your audience or presentation style.

- **Modular Code Structure:**
  - The repository is organized into separate modules for data preprocessing, transformation, index calculation, and visualization. This structure allows for easy extension and modification.

## Troubleshooting & Tips

- **Merge Variable Issues:**
  - Verify that the merge variable is formatted consistently between the Excel file and the shapefile (e.g., use leading zeros if necessary).

- **Data Format Errors:**
  - Ensure that all numerical indicators are properly formatted as numeric in the Excel file and that missing data handling thresholds are set appropriately.

- **Shapefile CRS:**
  - If the uploaded shapefile lacks a defined Coordinate Reference System (CRS), assign one using a GIS tool before uploading.

- **Weight Adjustments:**
  - Experiment with different weight settings to observe how changes impact the overall Vulnerability Index. Save different configurations for comparative analysis.

## Contributing

Contributions to improve functionality, add new features, or refine the methodology are welcome. Please follow these steps:

1. Fork the repository.
2. Create a new branch with your feature or fix.
3. Submit a pull request detailing your changes and the rationale behind them.

## License

*MIT License*

## Contact

For any questions, suggestions, or feedback, please feel free to reach out:

- **LinkedIn:** [Farhana Sarwar](https://www.linkedin.com/in/farhana-sarwar-813539133/) 
