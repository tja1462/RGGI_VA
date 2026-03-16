# RGGI_VA: Virginia and RGGI Analysis

Evaluation of CO2 emissions, carbon intensity, and capacity factors for fossil fuel power plants in Virginia and neighboring states following Virginia's entry into the Regional Greenhouse Gas Initiative (RGGI).

## Project Structure
- `data/`: Contains EPA eGRID plant-level data (2018-2023).
- `scripts/`: Data cleaning and analysis scripts (Python & R).
- `figures/`: Generated plots and visualizations.
- `tables/`: Statistical results in LaTeX format.

## Setup & Reproduction

### 1. Python Environment
Install dependencies:
```bash
pip install -r requirements.txt
```

### 2. R Environment
Ensure you have the following packages installed:
`tidyverse`, `ggthemes`, `fixest`, `patchwork`, `ggmap`, `scales`, `HonestDiD`, `purrr`.

### 3. Execution Pipeline
Run the scripts in the following order:

1. **Data Preparation**:
   - Execute `scripts/data_clean.ipynb` to merge raw eGRID files and generate `data/final_dataset.csv`.
2. **Analysis & Visualization**:
   - `scripts/EDA.R`: General exploratory data analysis and stacked bars.
   - `scripts/Parallel_Trends.R`: Parallel trends visualizations and HonestDiD sensitivity analysis.
   - `scripts/DiD.R`: Main Difference-in-Differences regression analysis.
   - `scripts/eda.ipynb`: Supplementary Python-based EDA.

## Reproducibility Notes
- All scripts use a fixed random seed (42) where applicable.
- All paths are relative to the project root.
- Python code handles eGRID encoding variations automatically.