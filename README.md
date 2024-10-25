
# COVID-19 Impact on Education

**Project by Jeong Kyu Lee**  
**Date: 2023-05-16**

## Overview
This project investigates the impact of COVID-19 on education outcomes across countries with different levels of GDP per capita. It addresses the research question:

> **How did the impact of COVID-19 on education differ across countries with different levels of GDP per capita?**

The project tests the hypothesis that countries with higher GDP per capita experienced less negative effects on education due to their greater capacity to transition to remote learning and implement effective education support measures. Two key indicators of education, youth literacy rate and gross secondary school enrollment rate, are analyzed using a linear regression model, including control variables like GNI per capita, unemployment rate, and urban population percentage.

## Key Features
- **Data Sources**: Data for this project is sourced from various international organizations, including the World Bank, WHO, and the United Nations. The data includes COVID-19 cases, education indicators, and socioeconomic factors from 2019-2021.
- **Regression Models**: The project uses linear regression to explore the relationship between GDP per capita, COVID-19 cases, and education outcomes, while controlling for several socioeconomic factors.
- **Scatter Plots**: The analysis includes scatter plots with linear regression lines to show how COVID-19 cases affect education in countries with the highest and lowest quartiles of GDP per capita.

## Key Files
- **`final_project.R`**: The main R script used to process the data, run the regression models, and create the visualizations.
- **`education.csv`, `wealth.csv`, `WHO-COVID-19-global-data.csv`, `population.csv`**: Datasets used for the analysis.
- **`README.md`**: This file, providing an overview of the project.

## How to Run
1. Install the required R packages (`dplyr`, `ggplot2`, `stargazer`, etc.) by running the following command in R:
   ```R
   install.packages(c("dplyr", "ggplot2", "stargazer", "dagitty", "countrycode", "lubridate", "gridExtra", "broom", "texreg"))
   ```
2. Load the project files and datasets into your working directory.
3. Run the `final_project.R` script to reproduce the regression analysis and visualizations.

## License
This project is open-source and available under the MIT License.
