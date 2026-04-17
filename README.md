# 📊 Women's Education & Population Growth: A Cross-National Statistical Analysis

A rigorous statistical investigation into the relationship between **women's educational attainment** and **annual population growth** across 185 countries in 2015, using Gapminder data. Includes full assumption testing, non-parametric alternatives, and 12 diagnostic visualisations.

---

## 🔍 Key Findings

| Analysis | Result |
|---|---|
| Spearman Correlation | ρ = −0.473, p < 0.001 (moderate negative relationship) |
| OLS Regression | β = −0.025, R² = 0.101, p < 0.001 |
| Kruskal-Wallis Test | χ² = 39.451, df = 3, p < 0.001 |

> Countries where women have relatively more education than men tend to have significantly lower population growth rates.

---

## 📐 Statistical Methods

### Linear Regression
- Fitted OLS regression of women's schooling on population growth
- Assumption testing: Shapiro-Wilk, Anderson-Darling (normality), Breusch-Pagan (homoscedasticity)
- Normality violated → switched to **Spearman's rank correlation** as non-parametric alternative

### Hypothesis Testing
- One-way ANOVA to test population growth differences across HDI categories
- Normality violated in 3/4 groups → switched to **Kruskal-Wallis test**
- Post-hoc pairwise comparisons: **Dunn's test with Bonferroni correction**

---

## 📁 Dataset

Data sourced from the **Gapminder Foundation** for 2015:

| Variable | Type | Description |
|---|---|---|
| Women's Mean Years in School (% of Men) | Ratio | Female-to-male schooling ratio, ages 25–34 |
| Annual Population Growth Rate (%) | Ratio | Country-level population growth |
| Human Development Index (HDI) | Ordinal | Categorised into Low / Medium / High / Very High |

185 countries included after listwise deletion of missing values.

---

## 📈 Figures Generated (12 total)

| Figure | Description |
|---|---|
| 1 | Distribution of Women's Mean Years in School |
| 2 | Distribution of Annual Population Growth |
| 3 | Population Growth by HDI Category (boxplot) |
| 4 | Women's Schooling vs Population Growth (scatter + regression) |
| 5 | Normal Q-Q Plot of Regression Residuals |
| 6 | Residuals vs Fitted Values |
| 7 | Scale-Location Plot |
| 8 | Histogram of Regression Residuals |
| 9 | Population Growth Distribution by HDI Group (faceted) |
| 10 | Normal Q-Q Plot of ANOVA Residuals |
| 11 | ANOVA Residuals vs Fitted Values |
| 12 | Histogram of ANOVA Residuals |

---

## 🛠 Libraries Used

```r
library(readxl)     # Data import
library(tidyverse)  # Data wrangling & ggplot2
library(car)        # Levene's test
library(lmtest)     # Breusch-Pagan test
library(nortest)    # Anderson-Darling test
library(dunn.test)  # Dunn's post-hoc test
```

---

## 📁 Repository Structure

```
womens-education-population-growth/
├── womens_education_population_analysis.R   # Full R script
├── images/                                  # All 12 generated figures
└── README.md
```

---

## 🚀 How to Run

1. Download the three Gapminder CSV/Excel files for 2015:
   - `hdi_human_development_index.xlsx`
   - `mean_years_in_school_women_percent_men_25_to_34_years.csv`
   - `population_growth_annual_percent.csv`
2. Place them in the same folder as the R script
3. Run `womens_education_population_analysis.R` in RStudio
4. All 12 figures will be saved to the `images/` folder automatically

---

## 👤 Author

**Yuvraj Bhatt**  
[LinkedIn](linkedin.com/in/yuvraj-bhatt-b9b5301b5) · [GitHub](yuvrajbhatt9865.url)

