# India Water Quality Analysis Project in R

## Overview
This R-based project delves into the critical issue of water quality in India, utilizing three datasets from Kaggle to analyze and predict water potability. It addresses India's ranking of 141 out of 180 countries in the Environmental Performance Index, highlighting the urgency of this issue.

## Datasets
1. `water_potability.csv`: Includes metrics like pH, Hardness, Solids, Chloramines, Sulfates, Turbidity, and potability status (True/False).
2. `waterQuality1.csv`: Features specific contaminants such as Lead, Mercury, Arsenic, Aluminium, bacteria, viruses, and potability status.
3. `water_dataX_new.csv`: Contains data from 2003 to 2014 across various Indian states, including pH, Nitrate & Nitrite, B.O.D., Fecal Coliform, Total Coliform, etc.

## Objectives
- **Potability Classification:** Train models to classify water samples as potable or not based on various contaminants.
- **Variable Correlation:** Analyze the importance of different variables in determining water safety, prioritizing checks for quicker remediation.
- **Scarcity Forecasting:** Use the third dataset to predict the timeline for severe drinking water scarcity in different Indian states.

## Key Features
- **Predictive Modeling:** Utilizing logistic regression, KNN, random forest, and XGBoost models in R for accurate classification and forecasting.
- **Data Visualization:** Creating insightful visualizations to represent the current state and future trends in water quality.
- **Actionable Insights:** Providing recommendations for immediate and long-term actions to improve water safety.

## R Scripts
- `Water Quality Analysis_Classification.R`
- `Water Quality Analysis_Regression.R`

## Tools and Technologies
- R (dplyr, ggplot2, caret, randomForest, xgboost)
- Kaggle Datasets

## Contributions
Contributions are welcome to enhance the project's scope and impact. Please refer to the contribution guidelines for more information.

## License
This project is distributed under the MIT License - see the LICENSE.md file for details.