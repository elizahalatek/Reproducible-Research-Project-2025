# Reproducible Research: Job Satisfaction Analysis

---

## Course Information
- **University**: *University of Warsaw, Faculty of Economic Sciences*
- **Course**: *Reproducible Research*
- **Project Title**: Job Satisfaction, happiness, and work-life balance: from R to Python
- **Authors**:
  - *Eliza Hałatek*
  - *Emilia Selwa*
  - *Bartłomiej Ramotowski*

---

## Project Objective

The aim of this project is to **reproduce and extend** a previous job satisfaction analysis that was originally conducted in **R**, by replicating it using **Python**. The workflow ensures full reproducibility and includes:
- Data cleaning and preprocessing.
- Exploratory Data Analysis (EDA) with visualizations.
- Statistical modeling (linear regression, ordered logistic and probit regression).
- Marginal effects estimation.
- Multicollinearity checks (Variance Inflation Factors).
- Comprehensive documentation and reproducible code.

---

## Libraries Used

The analysis relies on several Python libraries for data handling, modeling, and visualization:

### Data Handling
- `pandas` – for data manipulation and cleaning
- `numpy` – for numerical operations

### Visualization
- `matplotlib.pyplot` – for basic plotting
- `seaborn` – for enhanced visualizations and statistical plots

### Statistical Modeling
- `statsmodels` – for linear regression, probit and ordered logit models, marginal effects, and VIFs
- `mord` – for ordinal regression (LogisticAT model)
- `scipy` – for statistical tests including chi-squared

### Machine Learning Utilities
- `scikit-learn`:
  - `preprocessing.OrdinalEncoder` – to encode ordinal features
  - `compose.ColumnTransformer` – to apply preprocessing
  - `pipeline.make_pipeline` – for building ML pipelines
  - `model_selection.train_test_split` – to split datasets
  - `linear_model.LogisticRegression` – for comparison models
  - `metrics` – for model evaluation (accuracy, log loss)

### Others
- `warnings` – to manage warning messages and ensure cleaner outputs

---

## Repository Structure

| File                                            | Description                                                                                                               |
| ----------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| **EDA.ipynb**                                   | Exploratory Data Analysis (EDA) notebook: data inspection, visualization of key variables, and preliminary insights.      |
| **modeling.ipynb**                              | Statistical modeling notebook: linear regression, ordered logistic/probit models, marginal effects, and VIF calculations. |
| **AE model.R**                                  | Original R code used for the first modeling steps, including fixed likelihood ratio tests.                                |
| **ESS10SC-subset.csv**                          | Raw dataset: subset from the European Social Survey (ESS), Wave 10.                                                       |
| **clean_job_sat.csv**                           | Preprocessed and cleaned dataset ready for analysis.                                                                      |
| **job_satisfaction_data_preparation.ipynb**     | Notebook for data cleaning, handling missing values, and feature engineering.                                             |
| **Reproducible_Research_2025_Presentation.pdf** | Final presentation summarizing the project's objectives, methods, and results.                                            |
| **README.md**                                   | Project overview and structure (you are here!).                                                                           |

---

## AI Assistance Disclosure

This project team used ChatGPT, an AI language model based on GPT-4o-mini by OpenAI, to assist with translating and modifying code from R to Python. The AI support was limited to code adaptation and syntax improvements. 

---

## Codebook: Key Variables Description

### Job Satisfaction & Happiness
| Variable    | Description                                                            | Type    |
| ----------- | ---------------------------------------------------------------------- | ------- |
| **stfmjob** | Job satisfaction (0 = Extremely unsatisfied, 10 = Extremely satisfied) | Ordinal |
| **happy**   | General happiness (0 = Extremely unhappy, 10 = Extremely happy)        | Ordinal |

### Socioeconomic Variables
| Variable    | Description                            | Type         |
| ----------- | -------------------------------------- | ------------ |
| **inprdsc** | Number of close personal contacts      | Ordinal      |
| **health**  | Self-reported general health           | Ordinal      |
| **hlthhmp** | Hampered by illness/disability         | Binary       |
| **rlgdgr**  | Level of religiosity                   | Ordinal      |
| **brncntr** | Born in country (Yes/No)               | Binary       |
| **gndr**    | Gender                                 | Binary       |
| **agea**    | Age                                    | Quantitative |
| **rshpsts** | Relationship status                    | Qualitative  |
| **domicil** | Type of domicile                       | Qualitative  |
| **edulvlb** | Highest level of education             | Ordinal      |
| **eduyrs**  | Years of full-time education completed | Quantitative |

### Job-Related Variables
| Variable     | Description                                   | Type         |
| ------------ | --------------------------------------------- | ------------ |
| **uempla**   | Currently unemployed, actively looking        | Binary       |
| **uempli**   | Currently unemployed, not actively looking    | Binary       |
| **rtrd**     | Retired                                       | Binary       |
| **hswrk**    | Housework, childcare (last 7 days)            | Binary       |
| **emplrel**  | Type of employment relation                   | Qualitative  |
| **wrkctra**  | Type of work contract                         | Binary       |
| **estsz**    | Establishment size                            | Quantitative |
| **nacer2**   | Job industry classification                   | Qualitative  |
| **tporgwk**  | Organization type                             | Qualitative  |
| **uemp3m**   | Long-term unemployment experience (>3 months) | Binary       |
| **atncrse**  | Attended course/lecture in last 12 months     | Binary       |
| **hincsrca** | Main source of household income               | Qualitative  |
| **hinctnta** | Total net household income                    | Quantitative |

### Work-Life Balance Variables
| Variable     | Description                                       | Type         |
| ------------ | ------------------------------------------------- | ------------ |
| **wkdcorga** | Freedom to organize work schedule                 | Ordinal      |
| **wkhtot**   | Total weekly working hours                        | Quantitative |
| **emprelp**  | Employment status of partner                      | Qualitative  |
| **trdawrk**  | Tiredness after work (how often)                  | Ordinal      |
| **jbprtfp**  | Job prevents family time (how often)              | Ordinal      |
| **pfmfdjba** | Family pressure from respondent’s job (how often) | Ordinal      |
| **dcsfwrka** | Flexibility to start/finish work                  | Ordinal      |
