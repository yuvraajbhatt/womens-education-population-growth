"""
Women's Education & Population Growth
Cross-National Statistical Investigation Using Gapminder Data, 2015
====================================================================
Python equivalent of the R statistical analysis.

Author: Yuvraj Bhatt
Data:   Gapminder Foundation (https://www.gapminder.org)
Sample: 185 countries, 2015
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from scipy import stats
from scipy.stats import shapiro, kruskal, spearmanr
import statsmodels.api as sm
import statsmodels.formula.api as smf
from statsmodels.stats.diagnostic import het_breuschpagan
from scikit_posthocs import posthoc_dunn
import os

# Create images directory
os.makedirs("images", exist_ok=True)

# ============================================================
# SECTION 1: DATA LOADING & PREPARATION
# ============================================================

hdi_raw    = pd.read_excel("hdi_human_development_index.xlsx")
school_raw = pd.read_csv("mean_years_in_school_women_percent_men_25_to_34_years.csv")
pop_raw    = pd.read_csv("population_growth_annual_percent.csv")

# Extract 2015 columns
hdi_2015    = hdi_raw[["name", "2015"]].rename(columns={"2015": "hdi"}).dropna()
school_2015 = school_raw[["name", "2015"]].rename(columns={"2015": "school"}).dropna()
pop_2015    = pop_raw[["name", "2015"]].rename(columns={"2015": "pop_growth"}).dropna()

# Merge datasets
df = hdi_2015.merge(school_2015, on="name").merge(pop_2015, on="name")
df["hdi"] = pd.to_numeric(df["hdi"], errors="coerce")
df = df.dropna()

print(f"Sample size after merging: {len(df)} countries")
print(f"Missing values:\n{df.isnull().sum()}")

# HDI categories using UNDP cut-points
def categorise_hdi(hdi):
    if hdi < 0.550:
        return "1_Low"
    elif hdi < 0.700:
        return "2_Medium"
    elif hdi < 0.800:
        return "3_High"
    else:
        return "4_VeryHigh"

df["hdi_cat"] = df["hdi"].apply(categorise_hdi)
df["hdi_cat"] = pd.Categorical(df["hdi_cat"],
                                categories=["1_Low", "2_Medium", "3_High", "4_VeryHigh"],
                                ordered=True)

print("\nHDI Category Distribution:")
print(df["hdi_cat"].value_counts().sort_index())

# Summary statistics
print("\nTable 1: Summary Statistics")
print(df[["school", "pop_growth", "hdi"]].describe().round(3))

# ============================================================
# SECTION 2: EXPLORATORY DATA ANALYSIS
# ============================================================

COLORS = {
    "1_Low":      "#E53935",
    "2_Medium":   "#FB8C00",
    "3_High":     "#43A047",
    "4_VeryHigh": "#1E88E5"
}

# FIGURE 1: Histogram of Women's Schooling
fig, ax = plt.subplots(figsize=(8, 5))
ax.hist(df["school"], bins=25, color="#2196F3", edgecolor="white", alpha=0.85)
ax.axvline(df["school"].mean(),   color="red",       linestyle="--", linewidth=0.8, label=f"Mean = {df['school'].mean():.1f}")
ax.axvline(df["school"].median(), color="darkgreen", linestyle="--", linewidth=0.8, label=f"Median = {df['school'].median():.1f}")
ax.set_title("Figure 1: Distribution of Women's Mean Years in School (% of Men), 2015", fontweight="bold", fontsize=11)
ax.set_xlabel("Women's Mean Years in School (% of Men, Age 25-34)")
ax.set_ylabel("Number of Countries")
ax.legend()
plt.tight_layout()
plt.savefig("images/Figure1_hist_school.png", dpi=300)
plt.show()

# FIGURE 2: Histogram of Population Growth
fig, ax = plt.subplots(figsize=(8, 5))
ax.hist(df["pop_growth"], bins=25, color="#E53935", edgecolor="white", alpha=0.85)
ax.axvline(df["pop_growth"].mean(),   color="navy",       linestyle="--", linewidth=0.8, label=f"Mean = {df['pop_growth'].mean():.2f}")
ax.axvline(df["pop_growth"].median(), color="darkorange", linestyle="--", linewidth=0.8, label=f"Median = {df['pop_growth'].median():.2f}")
ax.set_title("Figure 2: Distribution of Annual Population Growth (%), 2015", fontweight="bold", fontsize=11)
ax.set_xlabel("Annual Population Growth (%)")
ax.set_ylabel("Number of Countries")
ax.legend()
plt.tight_layout()
plt.savefig("images/Figure2_hist_popgrowth.png", dpi=300)
plt.show()

# FIGURE 3: Boxplot by HDI Category
fig, ax = plt.subplots(figsize=(8, 5))
groups   = ["1_Low", "2_Medium", "3_High", "4_VeryHigh"]
labels   = ["Low", "Medium", "High", "Very High"]
data_grp = [df[df["hdi_cat"] == g]["pop_growth"].values for g in groups]
bp = ax.boxplot(data_grp, patch_artist=True, labels=labels)
for patch, grp in zip(bp["boxes"], groups):
    patch.set_facecolor(COLORS[grp])
    patch.set_alpha(0.75)
ax.set_title("Figure 3: Annual Population Growth (%) by HDI Category, 2015", fontweight="bold", fontsize=11)
ax.set_xlabel("HDI Category")
ax.set_ylabel("Annual Population Growth (%)")
plt.tight_layout()
plt.savefig("images/Figure3_boxplot_hdi.png", dpi=300)
plt.show()

# ============================================================
# SECTION 3: LINEAR REGRESSION
# ============================================================

# FIGURE 4: Scatter plot with regression line
fig, ax = plt.subplots(figsize=(8, 5))
ax.scatter(df["school"], df["pop_growth"], alpha=0.55, color="#1E88E5", s=30)

# OLS regression line
m, b = np.polyfit(df["school"], df["pop_growth"], 1)
x_line = np.linspace(df["school"].min(), df["school"].max(), 200)
ax.plot(x_line, m * x_line + b, color="red", linewidth=1.2, label="Linear fit")

ax.set_title("Figure 4: Women's Schooling vs. Annual Population Growth, 2015", fontweight="bold", fontsize=11)
ax.set_xlabel("Women's Mean Years in School (% of Men, Age 25-34)")
ax.set_ylabel("Annual Population Growth (%)")
ax.legend()
plt.tight_layout()
plt.savefig("images/Figure4_scatter_regression.png", dpi=300)
plt.show()

# Fit OLS model
X = sm.add_constant(df["school"])
lm_model = sm.OLS(df["pop_growth"], X).fit()
print("\nOLS Regression Summary:")
print(lm_model.summary())

residuals = lm_model.resid
fitted    = lm_model.fittedvalues

# Assumption tests
sw_stat, sw_p = shapiro(residuals)
print(f"\nShapiro-Wilk: W = {sw_stat:.3f}, p = {sw_p:.4f}")

bp_stat, bp_p, _, _ = het_breuschpagan(residuals, X)
print(f"Breusch-Pagan: BP = {bp_stat:.3f}, p = {bp_p:.4f}")

# Non-parametric alternative
rho, p_spearman = spearmanr(df["school"], df["pop_growth"])
print(f"\nSpearman Correlation: rho = {rho:.3f}, p = {p_spearman:.4f}")

# FIGURE 5: Q-Q Plot
fig, ax = plt.subplots(figsize=(7, 5))
sm.qqplot(residuals, line="s", ax=ax, alpha=0.7, color="#1E88E5")
ax.set_title("Figure 5: Normal Q-Q Plot of Regression Residuals", fontweight="bold", fontsize=11)
ax.set_xlabel("Theoretical Quantiles")
ax.set_ylabel("Sample Quantiles")
plt.tight_layout()
plt.savefig("images/Figure5_QQ_residuals.png", dpi=300)
plt.show()

# FIGURE 6: Residuals vs Fitted
fig, ax = plt.subplots(figsize=(7, 5))
ax.scatter(fitted, residuals, alpha=0.55, color="#1E88E5", s=30)
ax.axhline(0, color="red", linestyle="--", linewidth=0.9)
ax.set_title("Figure 6: Residuals vs Fitted Values", fontweight="bold", fontsize=11)
ax.set_xlabel("Fitted Values")
ax.set_ylabel("Residuals")
plt.tight_layout()
plt.savefig("images/Figure6_residuals_vs_fitted.png", dpi=300)
plt.show()

# FIGURE 7: Scale-Location Plot
sqrt_abs_resid = np.sqrt(np.abs(stats.zscore(residuals)))
fig, ax = plt.subplots(figsize=(7, 5))
ax.scatter(fitted, sqrt_abs_resid, alpha=0.55, color="#8E24AA", s=30)
ax.set_title("Figure 7: Scale-Location Plot", fontweight="bold", fontsize=11)
ax.set_xlabel("Fitted Values")
ax.set_ylabel("√|Standardised Residuals|")
plt.tight_layout()
plt.savefig("images/Figure7_scale_location.png", dpi=300)
plt.show()

# FIGURE 8: Histogram of Residuals
fig, ax = plt.subplots(figsize=(7, 5))
ax.hist(residuals, bins=20, density=True, color="#1E88E5", edgecolor="white", alpha=0.8)
x_norm = np.linspace(residuals.min(), residuals.max(), 200)
ax.plot(x_norm, stats.norm.pdf(x_norm, residuals.mean(), residuals.std()),
        color="darkgreen", linestyle="--", linewidth=1.2, label="Normal distribution")
ax.set_title("Figure 8: Histogram of Regression Residuals", fontweight="bold", fontsize=11)
ax.set_xlabel("Residuals")
ax.set_ylabel("Density")
ax.legend()
plt.tight_layout()
plt.savefig("images/Figure8_hist_residuals.png", dpi=300)
plt.show()

# ============================================================
# SECTION 4: HYPOTHESIS TESTING
# ============================================================
# H0: Median population growth is equal across all HDI groups
# H1: At least one HDI group has a different median

# Shapiro-Wilk per group
print("\nShapiro-Wilk by HDI Group:")
for grp in groups:
    grp_data = df[df["hdi_cat"] == grp]["pop_growth"]
    w, p = shapiro(grp_data)
    print(f"  {grp}: W = {w:.3f}, p = {p:.4f}")

# Kruskal-Wallis test
kw_stat, kw_p = kruskal(*[df[df["hdi_cat"] == g]["pop_growth"].values for g in groups])
print(f"\nKruskal-Wallis: chi-square = {kw_stat:.3f}, p = {kw_p:.4f}")

# Dunn's post-hoc test
print("\nDunn's Post-Hoc Test (Bonferroni):")
dunn_result = posthoc_dunn(df, val_col="pop_growth", group_col="hdi_cat", p_adjust="bonferroni")
print(dunn_result.round(4))

# FIGURE 9: Faceted Histograms by HDI Group
fig, axes = plt.subplots(2, 2, figsize=(10, 7))
axes = axes.flatten()
hdi_labels = {"1_Low": "Low HDI", "2_Medium": "Medium HDI",
              "3_High": "High HDI", "4_VeryHigh": "Very High HDI"}
for ax, grp in zip(axes, groups):
    grp_data = df[df["hdi_cat"] == grp]["pop_growth"]
    ax.hist(grp_data, bins=15, color=COLORS[grp], edgecolor="white", alpha=0.85)
    ax.set_title(hdi_labels[grp], fontweight="bold")
    ax.set_xlabel("Annual Population Growth (%)")
    ax.set_ylabel("Number of Countries")
fig.suptitle("Figure 9: Distribution of Population Growth by HDI Category, 2015",
             fontweight="bold", fontsize=11)
plt.tight_layout()
plt.savefig("images/Figure9_facet_hist_hdi.png", dpi=300)
plt.show()

# ANOVA residuals via OLS
anova_model = smf.ols("pop_growth ~ C(hdi_cat)", data=df).fit()
anova_resid  = anova_model.resid
anova_fitted = anova_model.fittedvalues

# FIGURE 10: Q-Q Plot of ANOVA Residuals
fig, ax = plt.subplots(figsize=(7, 5))
sm.qqplot(anova_resid, line="s", ax=ax, alpha=0.7, color="#E53935")
ax.set_title("Figure 10: Normal Q-Q Plot of ANOVA Residuals", fontweight="bold", fontsize=11)
plt.tight_layout()
plt.savefig("images/Figure10_QQ_anova_residuals.png", dpi=300)
plt.show()

# FIGURE 11: ANOVA Residuals vs Fitted
fig, ax = plt.subplots(figsize=(7, 5))
ax.scatter(anova_fitted, anova_resid, alpha=0.55, color="#E53935", s=30)
ax.axhline(0, color="navy", linestyle="--", linewidth=0.9)
ax.set_title("Figure 11: ANOVA Residuals vs Fitted Values", fontweight="bold", fontsize=11)
ax.set_xlabel("Fitted Values (Group Means)")
ax.set_ylabel("Residuals")
plt.tight_layout()
plt.savefig("images/Figure11_anova_residuals_vs_fitted.png", dpi=300)
plt.show()

# FIGURE 12: Histogram of ANOVA Residuals
fig, ax = plt.subplots(figsize=(7, 5))
ax.hist(anova_resid, bins=20, density=True, color="#E53935", edgecolor="white", alpha=0.8)
x_norm = np.linspace(anova_resid.min(), anova_resid.max(), 200)
ax.plot(x_norm, stats.norm.pdf(x_norm, anova_resid.mean(), anova_resid.std()),
        color="darkgreen", linestyle="--", linewidth=1.2, label="Normal distribution")
ax.set_title("Figure 12: Histogram of ANOVA Residuals", fontweight="bold", fontsize=11)
ax.set_xlabel("Residuals")
ax.set_ylabel("Density")
ax.legend()
plt.tight_layout()
plt.savefig("images/Figure12_hist_anova_residuals.png", dpi=300)
plt.show()

print("\nAll 12 figures saved to images/ folder.")
